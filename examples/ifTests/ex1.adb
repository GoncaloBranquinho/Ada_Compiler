procedure Main is
  score : Integer := 75;
begin
  if true then
    Put_Line("Aprovado");
  else
    if score >= 50 then
      Put_Line("Suficiente");
    else
      Put_Line("Reprovado");
    end if;
  end if;
end Main;
