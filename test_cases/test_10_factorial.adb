procedure Main is
    n : Integer := 5;
    resultado : Integer := 1;
    i : Integer := 1;
begin
    while i <= n loop
        resultado := resultado * i;
        i := i + 1;
    end loop;
    Put_Line(str(resultado));
end Main;
