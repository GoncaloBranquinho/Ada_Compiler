procedure main is
    b : boolean;
begin
    b := (3 < 5) and (10 /= 10);
    if b then
        put_line("True");
    else
        put_line("False");
    end if;
end main;
