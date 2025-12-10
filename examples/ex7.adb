procedure Main is
  z : Integer := 3;
  x : Integer := 0;
begin
  while (z > 0) loop
    z := z-1;
    x := x + 1;
  end loop;
  put_line(str(x));
end Main;
