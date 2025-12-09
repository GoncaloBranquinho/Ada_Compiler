procedure mAiN is
  x : Integer := -5;
  y : Integer := 10;
  b : Boolean := TRUE;
  result : Boolean;

begin
  result := x /= y;

  if result then
    Put_Line("Resultado verdadeiro");
  else
    Put_Line("Resultado falso");
  end if;
end mAiN;

