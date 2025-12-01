procedure Main is
    x : Integer;
    begin
        Put_Line("A resposta local é: ");
        declare
            y : Integer := 2;
            z : Integer := 3;
            begin
                x := (y + z);
                declare
                    a : Integer := 1;
                    b : Integer;
                    begin
                    end;
            end;
        Put_Line("A resposta pós local é: ");
    end Main;
