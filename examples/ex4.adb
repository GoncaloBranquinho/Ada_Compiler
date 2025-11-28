procedure Main is
    x : Integer := 5;
    begin
        Put_Line("A resposta local é: ");
        declare
            y : Integer := 2;
            z : Integer := 3;
            x : Integer := 4;
            begin
                x := (y + z);
                Put_Line(x);
            end;
        declare 
          k : Integer;
        begin

        end;
        Put_Line("A resposta pós local é: ");
        Put_Line(x);
    end Main;
