procedure Main is
    x : Integer := 10;
    y : Integer := 10;
    resultado : Boolean;
begin
    resultado := x <= y;
    if resultado then
        Put_Line("Menor ou igual");
    else
        Put_Line("Maior");
    end if;
end Main;
