procedure Main is
    x : Integer := 42;
    y : Integer := 42;
    resultado : Boolean;
begin
    resultado := x = y;
    if resultado then
        Put_Line("Igual");
    else
        Put_Line("Diferente");
    end if;
end Main;
