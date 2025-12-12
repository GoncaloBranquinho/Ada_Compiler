procedure main is
    i    : integer := 0;
    done : boolean := false;
begin
    while not done loop
        i := i + 1;
        if i = 3 then
            done := true;
        else
            done := false;
        end if;
    end loop;
    put_line(str(i));
end main;
