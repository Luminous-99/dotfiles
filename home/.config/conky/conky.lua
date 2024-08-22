-- Conky, a system monitor https://github.com/brndnmtthws/conky
--
-- This configuration file is Lua code. You can write code in here, and it will
-- execute when Conky loads. You can use it to generate your own advanced
-- configurations.
--
-- Try this (remove the `--`):
--
--   print("Loading Conky config")
--
-- For more on Lua, see:
-- https://www.lua.org/pil/contents.html

conky.config = {
    alignment = 'top_right',
    background = true,
    border_width = 1,
    cpu_avg_samples = 2,
    default_color = 'black',
    default_outline_color = 'black',
    default_shade_color = 'black',
    double_buffer = true,
    draw_borders = true,
    draw_graph_borders = true,
    draw_outline = false,
    draw_shades = false,
    extra_newline = false,
    font = 'Cascadia Code:size=10',
    gap_x = 10,
    gap_y = 10,
    minimum_height = 5,
    minimum_width = 5,
    net_avg_samples = 2,
    no_buffers = true,
    out_to_console = false,
    out_to_ncurses = false,
    out_to_stderr = false,
    out_to_x = true,
    own_window = true,
    own_window_colour = '#f0f0f0',
    own_window_transparent = false,
    own_window_argb_visual = true,
    own_window_argb_value = 129,
    own_window_class = 'Conky',
    own_window_type = 'override',
    show_graph_range = false,
    show_graph_scale = false,
    stippled_borders = 0,
    update_interval = 0.5,
    uppercase = false,
    use_spacer = 'none',
    use_xft = true,
}

conky.text = [[
${color black}Info:$color ${scroll 32 Conky $conky_version - $sysname $nodename $kernel $machine}
$hr
${color black}Uptime:$color $uptime
${color black}Frequency (in MHz):$color $freq
${color black}Frequency (in GHz):$color $freq_g
${color black}RAM Usage:$color $mem/$memmax - $memperc% ${membar 4}
${color black}Swap Usage:$color $swap/$swapmax - $swapperc% ${swapbar 4}
${color black}CPU Usage:$color $cpu% ${cpubar 4}
${color black}Processes:$color $processes  ${color black}Running:$color $running_processes
$hr
${color black}File systems:
 / $color${fs_used /}/${fs_size /} ${fs_bar 6 /}
${color black}Networking:
Up:$color ${upspeed} ${color black} - Down:$color ${downspeed}
$hr
${color black}Name              PID     CPU%   MEM%
${color black} ${top name 1} ${top pid 1} ${top cpu 1} ${top mem 1}
${color black} ${top name 2} ${top pid 2} ${top cpu 2} ${top mem 2}
${color black} ${top name 3} ${top pid 3} ${top cpu 3} ${top mem 3}
${color black} ${top name 4} ${top pid 4} ${top cpu 4} ${top mem 4}
]]
