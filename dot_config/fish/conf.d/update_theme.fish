function update_theme --on-variable theme_background
    if [ "$theme_background" = "dark" ]
        fish_config theme choose tokyonight_night
    else if [ "$theme_background" = "light" ]
        fish_config theme choose tokyonight_day
    end
end
