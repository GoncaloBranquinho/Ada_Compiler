procedure Main is
  x : Integer := 1;
begin
  while 10 > x loop
    x := x * 2;
  end loop;
  Put_Line(str(x));
end Main;
