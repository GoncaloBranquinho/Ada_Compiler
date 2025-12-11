procedure Main is
  I : Integer := 1;
  J : Integer := 1;
begin
  while I <= 2 loop
    J := 1;
    while J <= 2 loop
      Put_Line(str(I));
      Put_Line(str(J));
      J := J + 1;
    end loop;
    I := I + 1;
  end loop;
end Main;
