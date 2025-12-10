procedure Main is
  z : Integer := 0;
  x : Integer := 3;
begin
  while (x > 0) loop
    x := x-1;
    z := z+1;
  end loop;
  put_line(str(z));
end Main;

