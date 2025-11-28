procedure Main is
    x : Integer := 5;
    begin
        Put_Line("A resposta local é: ");
        declare
            y : Integer := 2;
            z : Integer := 3;
            begin
                x := (y + z);
                Get_Line(x,y);
            end;
        declare
          k : Integer;
        begin
            Put_Line(y);
            Put_Line(x);
        end;
        Put_Line("A resposta pós local é: ");
        Put_Line(x);
    end Main;
