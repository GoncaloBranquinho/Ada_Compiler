procedure Main is
    a : Boolean := True;
    b : Boolean := False;
    resultado : Boolean;
begin
    resultado := a or b;
    if resultado then
        Put_Line("Verdadeiro");
    else
        Put_Line("Falso");
    end if;
end Main;
