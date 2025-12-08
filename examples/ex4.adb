procedure Main is
    x : Integer := 5;
    begin
        Put_Line("A resposta local é: ");
        x := 7;
        declare
            a : Integer := 2;
            b : Integer := 3;
            x : Integer := 2;
            y : Integer := 3;
            p : String := "ola";
            begin
                x := a+b;
                y := a-b;
              
            end;
        declare
          k : String;
        begin
            k := "ola" & "adeus" & "oi";
            Put_Line(k);
        end;
        Put_Line("A resposta pós local é: ");
    end Main;
