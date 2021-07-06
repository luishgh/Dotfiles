local galaxyline = require "galaxyline"
local condition = require "galaxyline.condition"
local sections = galaxyline.section

local colors = {
    bg = "#504945",
    yellow = "#fabd2f",
    cyan = "#008080",
    darkblue = "#458588",
    green = "#b8bb26",
    aqua = "#8ec07c",
    orange = "#fe8019",
    purple = "#b16286",
    magenta = "#d3869b",
    grey = "#a89984",
    blue = "#83a598",
    red = "#fb4934"
}

sections.left[1] = {
    RaibowRed = {
        provider = function() return '▊ ' end,
        highlight = {colors.blue, colors.bg}
    }
}

sections.left[2] = {
    ViMode = {
        provider = function()
            -- auto change color according to vim mode
            local mode_color = {
                n      = colors.green,   -- NORMAL
                i      = colors.red,     -- INSERT
                v      = colors.yellow,  -- VISUAL
                [''] = colors.yellow,  -- V·Block
                V      = colors.yellow,  -- V·Line
                c      = colors.magenta, -- COMMAND
                no     = colors.gray,    -- N·Operator Pending
                s      = colors.orange,  -- Select
                S      = colors.orange,  -- S·Line
                [''] = colors.orange,  -- S·Block
                cv     = colors.red,     -- Vim Ex
                ce     = colors.red,     -- Ex
                r      = colors.cyan,    -- Prompt
                rm     = colors.cyan,    -- More
                ['r?'] = colors.cyan,    -- Confirm
                ['!']  = colors.red,     -- Shell
                t      = colors.red      -- TERMINAL
            }
            vim.api.nvim_command('hi GalaxyViMode guifg=' .. mode_color[vim.fn.mode()])
            return '  '
        end,
        highlight = {colors.red, colors.bg, 'bold'}
    }
}

sections.left[3] = {
    FileSize = {
        provider = 'FileSize',
        condition = condition.buffer_not_empty,
        highlight = {colors.fg,colors.bg},
    }
}

sections.left[4] = {
    FileIcon = {
        provider = 'FileIcon',
        condition = condition.buffer_not_empty,
        highlight = {require ('galaxyline.provider_fileinfo').get_file_icon_color,colors.bg},
    }
}

sections.left[5] = {
    FileName = {
        provider = 'FileName',
        condition = condition.buffer_not_empty,
        highlight = {colors.aqua,colors.bg,'bold'}
    }
}

sections.left[6] = {
    LineInfo = {
        provider = 'LineColumn',
        -- separator = ' ',
        separator_highlight = {'NONE',colors.bg},
        highlight = {colors.fg,colors.bg},
    },
}

sections.left[7] = {
    PerCent = {
        provider = 'LinePercent',
        separator = ' ',
        separator_highlight = {'NONE',colors.bg},
        highlight = {colors.fg,colors.bg,'bold'},
    }
}

sections.left[8] = {
    DiagnosticError = {
        provider = "DiagnosticError",
        icon = "  ",
        highlight = {colors.red, colors.bg}
    }
}
sections.left[9] = {
    DiagnosticWarn = {
        provider = "DiagnosticWarn",
        icon = "  ",
        highlight = {colors.yellow, colors.bg}
    }
}

sections.left[10] = {
    DiagnosticHint = {
        provider = "DiagnosticHint",
        icon = "  ",
        highlight = {colors.cyan, colors.bg}
    }
}

sections.left[11] = {
    DiagnosticInfo = {
        provider = "DiagnosticInfo",
        icon = "  ",
        highlight = {colors.blue, colors.bg}
    }
}

sections.right[1] = {
    GitIcon = {
    provider = function() return '  ' end,
    condition = condition.check_git_workspace,
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.aqua,colors.bg,'bold'},
  }
}

sections.right[2] = {
    GitBranch = {
    provider = 'GitBranch',
    condition = condition.check_git_workspace,
    highlight = {colors.aqua,colors.bg,'bold'},
  }
}

sections.right[3] = {
    DiffAdd = {
    provider = 'DiffAdd',
    condition = condition.hide_in_width,
    icon = '  ',
    highlight = {colors.green,colors.bg},
  }
}

sections.right[4] = {
    DiffModified = {
    provider = 'DiffModified',
    condition = condition.hide_in_width,
    icon = ' 柳',
    highlight = {colors.orange,colors.bg},
  }
}

sections.right[5] = {
    DiffRemove = {
    provider = 'DiffRemove',
    condition = condition.hide_in_width,
    icon = '  ',
    highlight = {colors.red,colors.bg},
  }
}
