procedure Main is
    x : Integer := 5;
    begin
        Put_Line("A resposta local é: ");
        declare
            y : Integer := 2;
            z : Integer := 3;
            k : String;
            v : Integer;
            begin
                x := (y + z);
                Get_Line(k,v);
            end;
        declare
          k : String;
        begin
            k := "ola";
            Put_Line(k);
        end;
        Put_Line("A resposta pós local é: ");
    end Main;
