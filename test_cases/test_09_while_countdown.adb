procedure Main is
    i : Integer := 5;
    resultado : Integer := 0;
begin
    while i > 0 loop
        resultado := i;
        i := i - 1;
    end loop;
    Put_Line(str(resultado));
end Main;
