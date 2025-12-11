procedure Main is
  A : Boolean := False;
  B : Boolean := True;
begin
  if (A or B) and not (A and B) then
    Put_Line(str(4));
  end if;
end Main;
