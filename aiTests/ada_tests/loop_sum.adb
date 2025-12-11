procedure Main is
  i : Integer := 1;
  s : Integer := 0;
begin
  while 5 > i loop
    s := s + i;
    i := i + 1;
  end loop;
  Put_Line(str(s));
end Main;
