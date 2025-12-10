procedure Main is
  z : String := "ola";
  x : Integer := 3;
begin
  while (x > 0) loop
    x := x-1;
    z := z&"ola";
  end loop;
  put_line(str(z));
end Main;

