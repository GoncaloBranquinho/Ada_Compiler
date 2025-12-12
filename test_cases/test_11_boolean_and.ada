procedure Main is
    a : Boolean := True;
    b : Boolean := True;
    resultado : Boolean;
begin
    resultado := a and b;
    if resultado then
        Put_Line("Verdadeiro");
    else
        Put_Line("Falso");
    end if;
end Main;
