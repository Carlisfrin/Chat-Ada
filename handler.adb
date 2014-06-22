--Programa hecho por Carlos Arevalo Jimenez
with Ada.Text_IO;
with Lista_Vecinos;
with Latest_Msgs;
with sender_buffering;

package body handler is

	package TIO renames Ada.Text_IO;
	package V renames Lista_Vecinos;
	package M renames Latest_Msgs;
	package SB renames sender_buffering;
	use type LLU.End_Point_Type;
	use type ASU.Unbounded_String;
	
	procedure Inserta_Msg (llave : AC.Time;
						contenido_ep : LLU.End_Point_Type;
						contenido_sec : T.Seq_N_T;
						no_enviar_a : LLU.End_Point_Type) is
		vecindario : V.Tipo_Todos_Eps;
		ultimo : Natural;
		contenido : SB.Value_T;
	begin
		V.Get_Lista(vecindario, ultimo);
		contenido.ep := contenido_ep;
		contenido.Seq_N := contenido_sec;
		contenido.P_Buffer := bufer'Access;
		for i in 1..V.max_vecinos loop
			if LLU.Is_Null(vecindario(i)) then
				contenido.destinations(i).ep := null;
				contenido.destinations(i).retries := 0;
			elsif vecindario(i) = no_enviar_a then
				contenido.destinations(i).ep := null;
				contenido.destinations(i).retries := 0;
			else
				contenido.destinations(i).ep := vecindario(i);
				contenido.destinations(i).retries := 10;
			end if;
		end loop;
		SB.Put(llave, contenido);
	end Inserta_Msg;
	
	procedure Enviador (k : AC.Time) is
		val : SB.Value_T;
		exito : Boolean;
		hora : AC.Time;
		contador : Natural;
	begin
		contador := 0;
		SB.Get(k, val, exito);
		if exito then
			for i in 1..val.destinations'Last loop
				if not LLU.Is_Null(val.destinations(i).ep) and then val.destinations(i).retries > 0 then
					LLU.Send(val.destinations(i).ep, val.P_Buffer);
					val.destinations(i).retries := val.destinations(i).retries - 1;
				else
					contador := contador + 1;
				end if;
			end loop;
		end if;
		SB.Delete(k);
		if contador < val.destinations'Last then
			hora := AC.Clock + plazo_retransmision;
			SB.Put(hora, val);
			TH.Set_Timed_Handler(hora, Enviador'Access);
		end if;
	end Enviador;
	
	procedure Borrado_Retardado (k : AC.Time) is
	begin
		M.Delete(nick_logout);
	end Borrado_Retardado;

	procedure Peer_Handler (From : LLU.End_Point_Type;
						  To : LLU.End_Point_Type;
						  P_Buffer: access LLU.Buffer_Type) is
	comentario  : ASU.Unbounded_String;
	tipo_mensaje : Message_Type;
	nick: ASU.Unbounded_String;
	ep_creador : LLU.End_Point_Type;
	secuen : T.Seq_N_T;
	ep_remite : LLU.End_Point_Type;
	ep_reject : LLU.End_Point_Type;
	borrar : Boolean;
	hecho : Boolean;
	hora : AC.Time;
	valor : SB.Value_T;
	begin
		LLU.Reset(bufer);
		tipo_mensaje := Message_Type'Input(P_Buffer);
		if tipo_mensaje /= Ack then
			ep_creador := LLU.End_Point_Type'Input(P_Buffer);
			secuen := T.Seq_N_T'Input(P_Buffer);
			ep_remite := LLU.End_Point_Type'Input(P_Buffer);
		end if;
		case tipo_mensaje is
			when Init =>
				ep_reject := LLU.End_Point_Type'Input(P_Buffer);
				nick := ASU.Unbounded_String'Input(P_Buffer);
				LLU.Reset(P_Buffer.all);
				if (not M.Mensaje_Repetido(secuen, nick) or M.Es_Init(nick)) and not M.Demasiado_Nuevo(secuen, nick) then
					M.Delete(nick);
					if ASU.To_String(Mi_Nick) = ASU.To_String(nick) then
						tipo_mensaje := Reject;
						Message_Type'Output(bufer'Access, tipo_mensaje);
						LLU.End_Point_Type'Output(bufer'Access, To);
						ASU.Unbounded_String'Output(bufer'Access, nick);
						LLU.Send(ep_reject, bufer'Access);
					else
						M.Add(secuen, nick);
						tipo_mensaje := Ack;
						Message_Type'Output(bufer'Access, tipo_mensaje);
						LLU.End_Point_Type'Output(bufer'Access, To);
						LLU.End_Point_Type'Output(bufer'Access, ep_creador);
						T.Seq_N_T'Output(bufer'Access, secuen);
						LLU.Send(ep_remite, bufer'Access);
						LLU.Reset(bufer);
						if V.Total_Vecinos /= 0 then
							tipo_mensaje := Init;
							Message_Type'Output(bufer'Access, tipo_mensaje);
							LLU.End_Point_Type'Output(bufer'Access, ep_creador);
							T.Seq_N_T'Output(bufer'Access, secuen);
							LLU.End_Point_Type'Output(bufer'Access, To);
							LLU.End_Point_Type'Output(bufer'Access, ep_reject);
							ASU.Unbounded_String'Output(bufer'Access, nick);
							hora := AC.Clock;
							Inserta_Msg(hora, ep_creador, secuen, ep_remite);
							TH.Set_Timed_Handler(hora, Enviador'Access);
						end if;
					end if;
				elsif M.Mensaje_Repetido(secuen, nick) then
					tipo_mensaje := Ack;
					Message_Type'Output(bufer'Access, tipo_mensaje);
					LLU.End_Point_Type'Output(bufer'Access, To);
					LLU.End_Point_Type'Output(bufer'Access, ep_creador);
					T.Seq_N_T'Output(bufer'Access, secuen);
					LLU.Send(ep_remite, bufer'Access);
				else
					if V.Total_Vecinos /= 0 then
						Message_Type'Output(bufer'Access, tipo_mensaje);
						LLU.End_Point_Type'Output(bufer'Access, ep_creador);
						T.Seq_N_T'Output(bufer'Access, secuen);
						LLU.End_Point_Type'Output(bufer'Access, To);
						LLU.End_Point_Type'Output(bufer'Access, ep_reject);
						ASU.Unbounded_String'Output(bufer'Access, nick);
						hora := AC.Clock;
						Inserta_Msg(hora, ep_creador, secuen, ep_remite);
						TH.Set_Timed_Handler(hora, Enviador'Access);
					end if;
				end if;
			when Confirm =>
				nick := ASU.Unbounded_String'Input(P_Buffer);
				LLU.Reset(P_Buffer.all);
				if not M.Mensaje_Repetido(secuen, nick) and not M.Demasiado_Nuevo(secuen, nick) then
					M.Add(secuen, nick);
					hecho := False;
					if ep_remite = ep_creador and then not V.Vecino_Repe(ep_creador) then
						V.Add(ep_creador, hecho);
					end if;
					TIO.New_Line;
					TIO.Put_Line("El usuario " & ASU.To_String(nick) & " ha entrado en el chat");
					TIO.Put(">>: ");
					if not V.Vecino_Repe(ep_creador) and ep_remite = ep_creador then
						TIO.New_Line;
						TIO.Put_Line("El usuario " & ASU.To_String(nick) & " no ha podido entrar en tu lista de vecinos porque has alcanzado el maximo de vecinos");
						TIO.Put(">>: ");
					end if;
					tipo_mensaje := Ack;
					LLU.Reset(bufer);
					Message_Type'Output(bufer'Access, tipo_mensaje);
					LLU.End_Point_Type'Output(bufer'Access, To);
					LLU.End_Point_Type'Output(bufer'Access, ep_creador);
					T.Seq_N_T'Output(bufer'Access, secuen);
					LLU.Send(ep_remite, bufer'Access);
					LLU.Reset(bufer);
					if V.Total_Vecinos > 1 then
						tipo_mensaje := Confirm;
						Message_Type'Output(bufer'Access, tipo_mensaje);
						LLU.End_Point_Type'Output(bufer'Access, ep_creador);
						T.Seq_N_T'Output(bufer'Access, secuen);
						LLU.End_Point_Type'Output(bufer'Access, To);
						ASU.Unbounded_String'Output(bufer'Access, nick);
						hora := AC.Clock;
						Inserta_Msg(hora, ep_creador, secuen, ep_remite);
						TH.Set_Timed_Handler(hora, Enviador'Access);	
					end if;
				elsif M.Mensaje_Repetido(secuen, nick) then
					tipo_mensaje := Ack;
					Message_Type'Output(bufer'Access, tipo_mensaje);
					LLU.End_Point_Type'Output(bufer'Access, To);
					LLU.End_Point_Type'Output(bufer'Access, ep_creador);
					T.Seq_N_T'Output(bufer'Access, secuen);
					LLU.Send(ep_remite, bufer'Access);
				else
					if V.Total_Vecinos /= 0 then
						Message_Type'Output(bufer'Access, tipo_mensaje);
						LLU.End_Point_Type'Output(bufer'Access, ep_creador);
						T.Seq_N_T'Output(bufer'Access, secuen);
						LLU.End_Point_Type'Output(bufer'Access, To);
						ASU.Unbounded_String'Output(bufer'Access, nick);
						hora := AC.Clock;
						Inserta_Msg(hora, ep_creador, secuen, ep_remite);
						TH.Set_Timed_Handler(hora, Enviador'Access);	
					end if;
				end if;
			when Writer =>
				nick := ASU.Unbounded_String'Input(P_Buffer);
				comentario := ASU.Unbounded_String'Input(P_Buffer);
				if not M.Mensaje_Repetido(secuen, nick) and not M.Demasiado_Nuevo(secuen, nick) then
					M.Add(secuen, nick);
					TIO.New_Line;
					TIO.Put_Line(ASU.To_String(nick) & " : " & ASU.To_String(comentario));
					TIO.Put(">>: ");
					tipo_mensaje := Ack;
					Message_Type'Output(bufer'Access, tipo_mensaje);
					LLU.End_Point_Type'Output(bufer'Access, To);
					LLU.End_Point_Type'Output(bufer'Access, ep_creador);
					T.Seq_N_T'Output(bufer'Access, secuen);
					LLU.Send(ep_remite, bufer'Access);
					if V.Total_Vecinos /= 0 then
						tipo_mensaje := Writer;
						LLU.Reset(bufer);
						Message_Type'Output(bufer'Access, tipo_mensaje);
						LLU.End_Point_Type'Output(bufer'Access, ep_creador);
						T.Seq_N_T'Output(bufer'Access, secuen);
						LLU.End_Point_Type'Output(bufer'Access, To);
						ASU.Unbounded_String'Output(bufer'Access, nick);
						ASU.Unbounded_String'Output(bufer'Access, comentario);
						hora := AC.Clock;
						Inserta_Msg(hora, ep_creador, secuen, ep_remite);
						TH.Set_Timed_Handler(hora, Enviador'Access);
					end if;
				elsif M.Mensaje_Repetido(secuen, nick) then
					tipo_mensaje := Ack;
					Message_Type'Output(bufer'Access, tipo_mensaje);
					LLU.End_Point_Type'Output(bufer'Access, To);
					LLU.End_Point_Type'Output(bufer'Access, ep_creador);
					T.Seq_N_T'Output(bufer'Access, secuen);
					LLU.Send(ep_remite, bufer'Access);
				else
					if V.Total_Vecinos /= 0 then
						Message_Type'Output(bufer'Access, tipo_mensaje);
						LLU.End_Point_Type'Output(bufer'Access, ep_creador);
						T.Seq_N_T'Output(bufer'Access, secuen);
						LLU.End_Point_Type'Output(bufer'Access, To);
						ASU.Unbounded_String'Output(bufer'Access, nick);
						ASU.Unbounded_String'Output(bufer'Access, comentario);
						hora := AC.Clock;
						Inserta_Msg(hora, ep_creador, secuen, ep_remite);
						TH.Set_Timed_Handler(hora, Enviador'Access);	
					end if;
				end if;
			when Logout =>
				nick := ASU.Unbounded_String'Input(P_Buffer);
				borrar := Boolean'Input(P_Buffer);
				LLU.Reset(P_Buffer.all);
				if not M.Mensaje_Repetido(secuen, nick) and not M.Demasiado_Nuevo(secuen, nick) and nick /= Mi_Nick then
					nick_logout := nick;
					hora := AC.Clock + (10 * plazo_retransmision * (1.0 + Duration(pct_perdidas) / 10));
					TH.Set_Timed_Handler(hora, Borrado_Retardado'Access);
					M.Add(secuen, nick);
					if ep_creador = ep_remite and borrar then
						V.Delete(ep_creador);
					end if;
					tipo_mensaje := Ack;
					Message_Type'Output(bufer'Access, tipo_mensaje);
					LLU.End_Point_Type'Output(bufer'Access, To);
					LLU.End_Point_Type'Output(bufer'Access, ep_creador);
					T.Seq_N_T'Output(bufer'Access, secuen);
					LLU.Send(ep_remite, P_Buffer);
					LLU.Reset(bufer);
					tipo_mensaje := Logout;
					Message_Type'Output(bufer'Access, tipo_mensaje);
					LLU.End_Point_Type'Output(bufer'Access, ep_creador);
					T.Seq_N_T'Output(bufer'Access, secuen);
					LLU.End_Point_Type'Output(bufer'Access, To);
					ASU.Unbounded_String'Output(bufer'Access, nick);
					Boolean'Output(bufer'Access, borrar);
					if borrar then
						TIO.New_Line;
						TIO.Put_Line("El usuario " & ASU.To_String(nick) & " ha abandonado el chat");
						TIO.Put_Line("Su registro se borrara en" & Duration'Image(10 * plazo_retransmision * (1.0 + Duration(pct_perdidas) / 10)) & " segundos");
						TIO.Put(">>: ");
					end if;
					if V.Total_Vecinos /= 0 then
						hora := AC.Clock;
						Inserta_Msg(hora, ep_creador, secuen, ep_remite);
						TH.Set_Timed_Handler(hora, Enviador'Access);	
					end if;
				elsif M.Mensaje_Repetido(secuen, nick) then
					tipo_mensaje := Ack;
					Message_Type'Output(bufer'Access, tipo_mensaje);
					LLU.End_Point_Type'Output(bufer'Access, To);
					LLU.End_Point_Type'Output(bufer'Access, ep_creador);
					T.Seq_N_T'Output(bufer'Access, secuen);
					LLU.Send(ep_remite, bufer'Access);
				else
					if V.Total_Vecinos /= 0 then
						Message_Type'Output(bufer'Access, tipo_mensaje);
						LLU.End_Point_Type'Output(bufer'Access, ep_creador);
						T.Seq_N_T'Output(bufer'Access, secuen);
						LLU.End_Point_Type'Output(bufer'Access, To);
						ASU.Unbounded_String'Output(bufer'Access, nick);
						Boolean'Output(bufer'Access, borrar);
						hora := AC.Clock;
						Inserta_Msg(hora, ep_creador, secuen, ep_remite);
						TH.Set_Timed_Handler(hora, Enviador'Access);
					end if;
				end if;
			when Ack =>
				ep_remite := LLU.End_Point_Type'Input(P_Buffer);
				ep_creador := LLU.End_Point_Type'Input(P_Buffer);
				secuen := T.Seq_N_T'Input(P_Buffer);
				SB.Get(ep_creador, secuen, hora, valor, hecho);
				if hecho then
					for i in 1..V.Total_Vecinos loop
						if not LLU.Is_Null(valor.destinations(i).ep) and then valor.destinations(i).ep = ep_remite then
							valor.destinations(i).retries := 0;
							valor.destinations(i).ep := null;
						end if;
					end loop;
					SB.Put(hora, valor);
				end if;
			when others =>
				TIO.New_Line;
				TIO.Put_Line("Se ha recibido un mensaje erroneo");
				TIO.Put(">>: ");
		end case;
	LLU.Reset(P_Buffer.all);
	LLU.Reset(bufer);
	end Peer_Handler;

end handler;