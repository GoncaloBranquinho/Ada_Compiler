procedure Main is
  x : Integer := 6;
begin
  while (x > 0) and (x > 3) loop
    Put_Line(str(x));
    x := x - 1;
  end loop;
end Main;
