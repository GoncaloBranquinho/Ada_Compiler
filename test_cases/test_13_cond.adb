procedure main is
    x : integer := 0;
begin
    if x < 0 then
        put_line("neg");
    else
        if x = 0 then
            put_line("zero");
        else
            put_line("pos");
        end if;
    end if;
end main;
