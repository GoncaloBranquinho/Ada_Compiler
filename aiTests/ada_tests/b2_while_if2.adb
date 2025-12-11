procedure Main is
  X : Integer := 6;
begin
  while X > 0 loop
    if X <= 3 then
      Put_Line(str(X));
    else
      Put_Line(0);
    end if;
    X := X - 1;
  end loop;
end Main;
