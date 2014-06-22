--Programa hecho por Carlos Arévalo Jiménez
package body latest_msgs is

	use type ASU.Unbounded_String;
	use type T.Seq_N_T;
	
	max_mensajes : constant Positive := 50;
	
	subtype Lista_Size is Positive range 1..max_mensajes;
	type Tipo_Mensaje is
	record
		nick : ASU.Unbounded_String;
		seq : T.Seq_N_T;
		ocupado : Boolean;
	end record;
	type Tipo_Lista is array (Lista_Size) of Tipo_Mensaje;

	protected M is
	
		procedure Inicializa;

		procedure Add (secuencia : in T.Seq_N_T;
					   nombre : in ASU.Unbounded_String);
                                                                       

		procedure Delete (nombre : in ASU.Unbounded_String);
		
		function Mensaje_Repetido (secuencia : T.Seq_N_T;
							    nombre : ASU.Unbounded_String)
							    return Boolean;
							    
		function Demasiado_Nuevo (secuencia : T.Seq_N_T;
							    nombre : ASU.Unbounded_String)
							    return Boolean;
							    
		function Es_Init (name : ASU.Unbounded_String)
					   return Boolean;
				   
		private
			lista : Tipo_Lista;
			mi_nick : ASU.Unbounded_String;
			proximo_hueco : Lista_Size;
	end M;

	protected body M is
	
		procedure Inicializa is
		begin
			for i in Lista_Size loop
				lista(i).nick := ASU.To_Unbounded_String("vacio");
				lista(i).seq := 1;
				lista(i).ocupado := False;
			end loop;
			proximo_hueco := 1;
		end Inicializa;

		procedure Add (secuencia : in T.Seq_N_T;
					   nombre : in ASU.Unbounded_String) is
		begin
			lista(proximo_hueco).seq := secuencia;
			lista(proximo_hueco).nick := nombre;
			lista(proximo_hueco).ocupado := True;
			if proximo_hueco = Lista_Size'Last then
				proximo_hueco := Lista_Size'First;
			else
				proximo_hueco := proximo_hueco + 1;
			end if;
		end Add;

		procedure Delete (nombre : in ASU.Unbounded_String) is
		begin
			for i in Lista_Size loop
				if ASU.To_String(lista(i).nick) = ASU.To_String(nombre) then
					lista(i).ocupado := False;
				end if;
			end loop;
		end Delete;
	
		function Mensaje_Repetido (secuencia : T.Seq_N_T;
							    nombre : ASU.Unbounded_String)
							    return Boolean is
			repetido : Boolean;
		begin
			repetido := False;
			for i in Lista_Size loop
				if lista(i).ocupado and lista(i).seq >= secuencia and ASU.To_String(lista(i).nick) = ASU.To_String(nombre) then
					repetido := True;
				end if;
			end loop;
			return repetido;
		end Mensaje_Repetido;
		
		function Demasiado_Nuevo (secuencia : T.Seq_N_T;
							    nombre : ASU.Unbounded_String)
							    return Boolean is
			maxim : T.Seq_N_T;
			primer_msg : Boolean;
		begin
			maxim := 0;
			primer_msg := True;
			for i in Lista_Size loop
				if lista(i).ocupado and then ASU.To_String(lista(i).nick) = ASU.To_String(nombre) then
					primer_msg := False;
					if maxim < lista(i).seq then
						maxim := lista(i).seq;
					end if;
				end if;
			end loop;
			if primer_msg then
				return False;
			else
				return secuencia > T.Seq_N_T'Succ(maxim);
			end if;
		end Demasiado_Nuevo;
		
		function Es_Init (name : ASU.Unbounded_String)
					    return Boolean is
			contador : Natural;
		begin
			contador := 0;
			for i in Lista_Size loop
				if lista(i).ocupado and lista(i).nick = name and lista(i).seq > T.Seq_N_T'Succ(T.Seq_N_T'First) then
					contador := contador + 1;
				end if;
			end loop;
			return contador = 1;
		end Es_Init;

	end M;
	
	procedure Inicializa is
	begin
		M.Inicializa;
	end Inicializa;

	procedure Add (secuencia : in T.Seq_N_T;
				   nombre : in ASU.Unbounded_String) is
	begin
		M.Add (secuencia, nombre);
	end Add;

	procedure Delete (nombre : in ASU.Unbounded_String) is
	begin
		M.Delete (nombre);
	end Delete;
	
	function Mensaje_Repetido (secuencia : T.Seq_N_T;
						    nombre : ASU.Unbounded_String)
						    return Boolean is
	begin
		return M.Mensaje_Repetido(secuencia, nombre);
	end Mensaje_Repetido;
	
	function Demasiado_Nuevo (secuencia : T.Seq_N_T;
						    nombre : ASU.Unbounded_String)
						    return Boolean is
	begin
		return M.Demasiado_Nuevo(secuencia, nombre);
	end Demasiado_Nuevo;
	
	function Es_Init (name : ASU.Unbounded_String)
				   return Boolean is
	begin
		return M.Es_Init(name);
	end Es_Init;

end latest_msgs;