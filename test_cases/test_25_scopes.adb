procedure main is
    flag : boolean := true;
begin
    if flag then
        put_line("outer true");
    else
        put_line("outer false");
    end if;

    declare
        flag : boolean := false;
    begin
        if flag then
            put_line("inner true");
        else
            put_line("inner false");
        end if;

        declare
            flag : boolean := true;
        begin
            if flag then
                put_line("inner-inner true");
            else
                put_line("inner-inner false");
            end if;
        end;
    end;

    if flag then
        put_line("outer again true");
    else
        put_line("outer again false");
    end if;
end main;

