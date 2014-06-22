--Programa hecho por Carlos Arévalo Jiménez
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Lower_Layer_UDP;
with Ada.Exceptions;
with handler;
with latest_msgs;
with lista_vecinos;
with types;
with Timed_Handlers;
with Ada.Calendar;
with sender_buffering;

procedure Peer_Chat_2 is

	package ASU renames Ada.Strings.Unbounded;
	package TIO renames Ada.Text_IO;
	package ACL renames Ada.Command_Line;
	package LLU renames Lower_Layer_UDP;
	package M renames latest_msgs;
	package V renames lista_vecinos;
	package H renames handler;
	package T renames types;
	package TH renames Timed_Handlers;
	package AC renames Ada.Calendar;
	package SB renames sender_buffering;

	procedure LeeVecinos (numero_vec : Natural;
					    repeat : in out Boolean) is
		vec_ep : LLU.End_Point_Type;
		maq : ASU.Unbounded_String;
		port : Integer;
		vec_ip : ASU.Unbounded_String;
		done : Boolean;
		arg1 : Natural;
		arg2 : Natural;
	begin
		arg1 := 6;
		arg2 := 7;
		if repeat then
			arg1 := 8;
			arg2 := 9;
		end if;
		maq := ASU.To_Unbounded_String(ACL.Argument(arg1));
		vec_ip := ASU.To_Unbounded_String(LLU.To_Ip(ASU.To_String(maq)));
		port := Integer'Value(ACL.Argument(arg2));
		vec_ep := LLU.Build(ASU.To_String(vec_ip), port);
		V.Add(vec_ep, done);
		repeat := (arg1 = 6) and (numero_vec = 2);
		if repeat then
			LeeVecinos (numero_vec, repeat);
		end if;
	end LeeVecinos;
	
	procedure LeeLlamada (end_point : out LLU.End_Point_Type;
						nombre : out ASU.Unbounded_String;
						num_vecinos : out Natural) is
		ip: ASU.Unbounded_String;
		puerto: Integer;
		maquina: ASU.Unbounded_String;
		siempre_falso : Boolean;
	begin
		siempre_falso := False;
		maquina := ASU.To_Unbounded_String(LLU.Get_Host_Name);
		ip := ASU.To_Unbounded_String(LLU.To_Ip(ASU.To_String(maquina)));
		puerto := Integer'Value(ACL.Argument(1));
		end_point := LLU.Build(ASU.To_String(ip), puerto);
		nombre := ASU.To_Unbounded_String(ACL.Argument(2));
		H.delay_menor := Natural'Value(ACL.Argument(3));
		H.delay_mayor := Natural'Value(ACL.Argument(4));
		H.pct_perdidas := Natural'Value(ACL.Argument(5));
		num_vecinos := 0;
		if ACL.Argument_Count = 7 then
			num_vecinos := 1;
		elsif ACL.Argument_Count = 9 then
			num_vecinos := 2;
		end if;
		if num_vecinos /= 0 then
			LeeVecinos (num_vecinos, siempre_falso);
		end if;
		while H.delay_mayor < H.delay_menor loop
			TIO.Put_Line("Escribe un delay maximo mayor o igual que el minimo" & Natural'Image(H.delay_menor));
			H.delay_mayor := Natural'Value(TIO.Get_Line);
		end loop;
		while H.pct_perdidas > 99 or H.pct_perdidas < 0 loop
			TIO.Put_Line("Escribe un porcentaje de paquetes perdidos entre 0 y 99");
			H.pct_perdidas := Natural'Value(TIO.Get_Line);
		end loop;
	end;

	procedure EnviarLogout (h_ep : LLU.End_Point_Type;
						nickname : ASU.Unbounded_String;
						secuen : T.Seq_N_T;
						confirm_sent : Boolean) is
		tipo_mensaje : H.Message_Type;
		hora : AC.Time;
		contador : Natural;
	begin
		LLU.Reset(H.bufer);
		tipo_mensaje := H.Logout;
		H.Message_Type'Output(H.bufer'Access, tipo_mensaje);
		LLU.End_Point_Type'Output(H.bufer'Access, h_ep);
		T.Seq_N_T'Output(H.bufer'Access, secuen);
		LLU.End_Point_Type'Output(H.bufer'Access, h_ep);
		ASU.Unbounded_String'Output(H.bufer'Access, nickname);
		Boolean'Output(H.bufer'Access, confirm_sent);
		hora := AC.Clock;
		H.Inserta_Msg(hora, h_ep, secuen, h_ep);
		TH.Set_Timed_Handler(hora, H.Enviador'Access);
		contador := 0;
		TIO.Put_Line("Finalizando...");
		loop
			delay H.plazo_retransmision;
			contador := contador + 1;
		exit when SB.Is_Empty or contador = 11;
		end loop;
	end EnviarLogout;
	
	procedure MensajeInicial (handler_end_point: LLU.End_Point_Type;
						nombre: ASU.Unbounded_String;
						acogido: out Boolean;
						seq : in out T.Seq_N_T) is
						
		use type H.Message_Type;
		
		recibe_ep: LLU.End_Point_Type;
		tipo_mensaje: H.Message_Type;
		rejecter : ASU.Unbounded_String;
		expired : Boolean;
		hora : AC.Time;
	begin
		tipo_mensaje := H.Init;
		LLU.Bind_Any(recibe_ep);
		LLU.Reset(H.bufer);
		H.Message_Type'Output(H.bufer'Access, tipo_mensaje);
		LLU.End_Point_Type'Output(H.bufer'Access, handler_end_point);
		T.Seq_N_T'Output(H.bufer'Access, seq);
		LLU.End_Point_Type'Output(H.bufer'Access, handler_end_point);
		LLU.End_Point_Type'Output(H.bufer'Access, recibe_ep);
		ASU.Unbounded_String'Output(H.bufer'Access, nombre);
		M.Add(seq, nombre);
		hora := AC.Clock;
		H.Inserta_Msg(hora, handler_end_point, seq, handler_end_point);
		TH.Set_Timed_Handler(hora, H.Enviador'Access);
		LLU.Reset(H.bufer);
		LLU.Receive(recibe_ep, H.bufer'Access, 2.0, expired);
		acogido := True;
		if not expired then
			tipo_mensaje := H.Message_Type'Input(H.bufer'Access);
			if tipo_mensaje = H.Reject then
				seq := T.Seq_N_T'Succ(seq);
				recibe_ep := LLU.End_Point_Type'Input(H.bufer'Access);
				rejecter := ASU.Unbounded_String'Input(H.bufer'Access);
				acogido := False;
				EnviarLogout(handler_end_point, nombre, seq, acogido);
			end if;
		end if;
	end MensajeInicial;
	
	procedure EnviarConfirmacion(h_ep : LLU.End_Point_Type;
							 nickname : ASU.Unbounded_String;
							 secuen : T.Seq_N_T) is
		tipo_mensaje : H.Message_Type;
		hora : AC.Time;
	begin
		LLU.Reset(H.bufer);
		tipo_mensaje := H.Confirm;
		H.Message_Type'Output(H.bufer'Access, tipo_mensaje);
		LLU.End_Point_Type'Output(H.bufer'Access, h_ep);
		T.Seq_N_T'Output(H.bufer'Access, secuen);
		LLU.End_Point_Type'Output(H.bufer'Access, h_ep);
		ASU.Unbounded_String'Output(H.bufer'Access, nickname);
		M.Add(secuen, nickname);
		hora := AC.Clock;
		H.Inserta_Msg(hora, h_ep, secuen, h_ep);
		TH.Set_Timed_Handler(hora, H.Enviador'Access);
	end EnviarConfirmacion;
	
	procedure Chatea(handler_end_point : LLU.End_Point_Type; 
					nombre : ASU.Unbounded_String;
					seq : in out T.Seq_N_T) is
		mensaje : ASU.Unbounded_String;
		tipo_mensaje : H.Message_Type;
		confirmado : Boolean;
		hora : AC.Time;
	begin
		M.Delete(nombre);
		TIO.Put_Line("Escribe .salir para salir");
		loop
			TIO.Put(">>: ");
			mensaje := ASU.To_Unbounded_String(TIO.Get_Line);
			if ASU.To_String(mensaje) /= ".salir" then
				tipo_mensaje := H.Writer;
				LLU.Reset(H.bufer);
				H.Message_Type'Output(H.bufer'Access, tipo_mensaje);
				LLU.End_Point_Type'Output(H.bufer'Access, handler_end_point);
				T.Seq_N_T'Output(H.bufer'Access, seq);
				LLU.End_Point_Type'Output(H.bufer'Access, handler_end_point);
				ASU.Unbounded_String'Output(H.bufer'Access, nombre);
				ASU.Unbounded_String'Output(H.bufer'Access, mensaje);
				M.Add(seq, nombre);
				hora := AC.Clock;
				H.Inserta_Msg(hora, handler_end_point, seq, handler_end_point);
				TH.Set_Timed_Handler(hora, H.Enviador'Access);
				seq := T.Seq_N_T'Succ(seq);
			end if;
		exit when ASU.To_String(mensaje) = ".salir";
		end loop;
		confirmado := True;
		EnviarLogout(handler_end_point, nombre, seq, confirmado);
	end Chatea;
	
	nick : ASU.Unbounded_String;	
	handler_ep : LLU.End_Point_Type;
	aceptado : Boolean;
	vecinos : Natural;
	secuencial : T.Seq_N_T;
	
begin
	V.Inicializar_Lista;
	M.Inicializa;
	secuencial := T.Seq_N_T'First;
	LeeLlamada(handler_ep, nick, vecinos);
	H.Mi_Nick := nick;
	LLU.Set_Faults_Percent(H.pct_perdidas);
	LLU.Set_Random_Propagation_Delay (H.delay_menor, H.delay_mayor);
	H.plazo_retransmision := Duration(H.delay_mayor) * 2 / 1000;
	LLU.Bind(handler_ep, H.Peer_Handler'Access);
	if vecinos /= 0 then
		MensajeInicial(handler_ep, nick, aceptado, secuencial);
	else
		aceptado := True;
	end if;
	if aceptado then
		secuencial := T.Seq_N_T'Succ(secuencial);
		if vecinos /= 0 then
			EnviarConfirmacion(handler_ep, nick, secuencial);
			secuencial := T.Seq_N_T'Succ(secuencial);
		end if;
		TIO.Put_Line("Entras en el chat con el nick : " & ASU.To_String(nick));
		Chatea(handler_ep, nick, secuencial);
	else
		TIO.Put_Line("Has sido rechazado: tu nick ya esta siendo utilizado por otro usuario");
	end if;
	TH.Finalize;
	LLU.Finalize;
	exception
		when Ex:others =>
			Ada.Text_IO.Put_Line ("Excepcion imprevista: " & Ada.Exceptions.Exception_Name(Ex) & " en: " & Ada.Exceptions.Exception_Message(Ex));
			LLU.Finalize;
end Peer_Chat_2;