procedure Main is
  x : Integer := 2;
  y : Integer := 2;
begin
  while x > 0 loop
    y := 2;
    while y > 0 loop
      Put_Line(str(y));
      y := y - 1;
    end loop;
    x := x - 1;
  end loop;
end Main;
