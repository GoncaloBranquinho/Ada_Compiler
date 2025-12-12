procedure Main is
    i : Integer := 1;
    soma : Integer := 0;
begin
    while i <= 5 loop
        soma := soma + i;
        i := i + 1;
    end loop;
    Put_Line(str(soma));
end Main;
