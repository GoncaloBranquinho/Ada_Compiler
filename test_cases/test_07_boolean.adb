procedure main is
    b : boolean;
begin
    b := not (4 >= 4);
    if b then
        put_line("True");
    else
        put_line("False");
    end if;
end main;
