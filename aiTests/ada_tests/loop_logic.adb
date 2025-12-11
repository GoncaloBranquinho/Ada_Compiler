procedure Main is
  x : Integer := 5;
begin
  while x > 0 loop
    if x > 3 then
       Put_Line(str(x));
    else
      Put_Line(0);
    end if;
    x := x - 1;
  end loop;
end Main;
