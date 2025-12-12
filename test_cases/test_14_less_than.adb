procedure Main is
    x : Integer := 10;
    y : Integer := 20;
    resultado : Boolean;
begin
    resultado := x < y;
    if resultado then
        Put_Line("Menor");
    else
        Put_Line("Não menor");
    end if;
end Main;
