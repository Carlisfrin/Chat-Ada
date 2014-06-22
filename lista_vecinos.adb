--Programa hecho por Carlos Arévalo Jiménez
package body lista_vecinos is

	use type LLU.End_Point_Type;
   
	type Tipo_Elemento is
	record
		ep : LLU.End_Point_Type;
		ocupado : Boolean;
	end record;
	
	type Tipo_Lista is array (1..max_vecinos ) of Tipo_Elemento;

	protected V is
		procedure Inicializar_Lista;
		procedure Add (end_point : in LLU.End_Point_Type;
					    added : out Boolean);
		procedure Delete (end_point : in LLU.End_Point_Type);
		procedure Get_Lista (todos : out Tipo_Todos_Eps;
						   cantidad : out Natural);
		function Total_Vecinos return Natural;
		function Vecino_Repe (end_point : LLU.End_Point_Type) return Boolean;
		private
			lista : Tipo_Lista;
			list_length : Natural := 0;
	end V;

	protected body V is
   
		procedure Inicializar_Lista is
		begin
			for i in 1..max_vecinos loop
				lista(i).ocupado := False;
			end loop;
		end Inicializar_Lista;

		procedure Add (end_point : in LLU.End_Point_Type;
					added : out Boolean) is
			I : Integer;
		begin
		added := False;
		if list_length < lista'Size then
			I := lista'First;
			while I <= lista'Last and not added loop
				if not lista(I).ocupado then
					lista(I).ep := end_point;
					lista(I).ocupado  := True;
					list_length := list_length + 1;
					added := True;
				end if;
			I := I + 1;
			end loop;
		end if;
		end Add;

		procedure Delete (end_point : in LLU.End_Point_Type) is
			I : Positive;
		begin
			if list_length > 0 then
				I := lista'First;
				while I <= lista'Last loop
					if lista(I).ocupado and then lista(I).ep = end_point then
						lista(I).ocupado := False;
						list_length := list_length - 1;
					end if;
				I := I + 1;
				end loop;
			end if;
		end Delete;
		
		procedure Get_Lista (todos : out Tipo_Todos_Eps;
						   cantidad : out Natural) is
		begin
			cantidad := 0;
			for i in 1..max_vecinos loop
				if lista(i).ocupado then
					cantidad := cantidad + 1;
					todos(cantidad) := lista(i).ep;
				end if;
			end loop;
		end Get_Lista;
		
		function Total_Vecinos return Natural is
			respuesta : Natural;
		begin
			respuesta := 0;
			for i in 1..max_vecinos loop
				if lista(i).ocupado then
					respuesta := respuesta + 1;
				end if;
			end loop;
			return respuesta;
		end Total_Vecinos;
		
		function Vecino_Repe (end_point : LLU.End_Point_Type) return Boolean is
			repe : Boolean;
		begin
			repe := False;
			for i in 1..Total_Vecinos loop
				if lista(i).ep = end_point then
					repe := True;
				end if;
			end loop;
			return repe;
		end Vecino_Repe;

	end V;
	
	procedure Inicializar_Lista is
	begin
		V.Inicializar_Lista;
	end Inicializar_Lista;

	procedure Add (end_point : in LLU.End_Point_Type;
				    added : out Boolean) is
	begin
		V.Add (end_point, added);
	end Add;


	procedure Delete (end_point : in LLU.End_Point_Type) is
	begin
		V.Delete (end_point);
	end Delete;
	
	procedure Get_Lista (todos : out Tipo_Todos_Eps;
					  cantidad : out Natural) is
	begin
		V.Get_Lista (todos, cantidad);
	end Get_Lista;
	
	function Total_Vecinos return Natural is
	begin
		return V.Total_Vecinos;
	end Total_Vecinos;
	
	function Vecino_Repe (end_point : LLU.End_Point_Type) return Boolean is
	begin
		return V.Vecino_Repe(end_point);
	end Vecino_Repe;

end lista_vecinos;