"    Copyright 2011 Timothy Mellor
"
"    This program is free software: you can redistribute it and/or modify
"    it under the terms of the GNU General Public License as published by
"    the Free Software Foundation, either version 3 of the License, or
"    (at your option) any later version.
"
"    This program is distributed in the hope that it will be useful,
"    but WITHOUT ANY WARRANTY; without even the implied warranty of
"    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"    GNU General Public License for more details.
"
"    You should have received a copy of the GNU General Public License
"    along with this program.  If not, see <http://www.gnu.org/licenses/>.


" A very simple plugin to work with extempore.
" In general, this could be a LOT faster, and much more
" elegant. But for now, it works :).
" 
" INSTALL: 
"  Option 1. Open vim, run ":source /path/to/extempore.vim". This is not
"     permanent!
"  Option 2. Place extempore.vim in your vim plugin directory.
"
" TODO:
"   - allow vim command to specify host and port
"   - speed up algorithm for finding enclosing block
"   - add an indicator that a command was sent, maybe an underline or flash
"   - clean up code a bit

" Define all of the Extempore commands
command! -nargs=* ExtemporeOpenConnection :python open()
command! -nargs=* ExtemporeCloseConnection :python close()
command! -nargs=* ExtemporeSendEnclosingBlock :python send_enclosing_block()
command! -nargs=* ExtemporeSendEntireFile :python send_entire_file()
command! -nargs=* ExtemporeSendSelection :python send_selection()

" These are the mappings I found to be convenient, which
" didn't interfere with my own personal setup. Feel free to change them.
nnoremap <Leader>o :ExtemporeOpenConnection() <CR>
nnoremap <Leader>x :ExtemporeCloseConnection() <CR>
nnoremap <Leader>w :ExtemporeSendEnclosingBlock() <CR>
nnoremap <Leader>a :ExtemporeSendEntireFile() <CR>
nnoremap <Leader>s :ExtemporeSendSelection() <CR>

python << EOF
import vim
import telnetlib

HOST = "localhost"
PORT = 7099

telnet = None

def open():
    """ Opens the connection """
    global telnet
    telnet = telnetlib.Telnet(HOST, PORT)

def close():
    global telnet
    if telnet:
        telnet.close()

def send_string(value):
    """ Sends the desired string through the connection """
    global telnet
    if not telnet:
        print "Not connected"
        return
    if value:
        telnet.write(value)

def send_enclosing_block():
    """ Grab the enclosing function block and send it, ie if you
        are inside a (define ...) somewhere, we want to send that."""
    send_string(get_enclosing_block())

def send_entire_file():
    send_string(get_entire_file())

def send_selection():
    """ Send the text determined by the '<' and '>' marks. """
    send_string(get_selection())

def get_entire_file():
    lines = vim.current.buffer
    result = join_lines(lines)
    return result

def get_selection():
    lines = vim.current.buffer
    start_selection, col = vim.current.buffer.mark("<")
    # vim index is not 0 based, facepalm.jpg
    start_selection -= 1
    end_selection, col = vim.current.buffer.mark(">")

    result = join_lines(lines[start_selection:end_selection])

    return result

def get_enclosing_block():
    current_line, current_col = vim.current.window.cursor
    # facepalm.jpg, really vim?
    current_line -= 1
    buffer_lines = vim.current.buffer
    result = get_enclosing_block_line_numbers(current_line, buffer_lines)
    if result is None:
        return None
    start_line, end_line = result

    result = join_lines(vim.current.buffer[start_line:end_line+1])
    return result


def get_enclosing_block_line_numbers(line_num, lines):
    """ Given the current line number, and a list of lines representing
        the buffer, return the line indexes of the beginning and end of
        the current block. 
        
        Steps:
            1. Go through previous lines, find one which matches '^(', ie 
                start of line is a paren. Call this line top_placeholder.
            2. From top_placeholder, go towards bottom of file until left and
                right parent counts are equal. Call this line
                bottom_placeholder
            3. Return the tuple (top, bottom) placeholders as long as the
                current line resides in them. Else, return None.
            """
    top_placeholder = line_num
    # lines from current to beginning, reversed
    for line in lines[:line_num+1][::-1]:
        if line.startswith("("):
            break
        top_placeholder -= 1

    left_parens = 0
    right_parens = 0
    bottom_placeholder = top_placeholder
    # lines from top_placeholder to end
    for line in lines[top_placeholder:]:
        left_parens += line.count("(")
        right_parens += line.count(")")
        if left_parens == right_parens:
            break
        bottom_placeholder += 1

    # if entire code block is above the current line, return None
    if bottom_placeholder < line_num:
        return None
    else:
        return (top_placeholder, bottom_placeholder)

def join_lines(lines):
    """ Join lines by spaces, remove any comments, and end with newline"""
    result = ""
    for line in lines:
        # remove comment; TODO: do less hacky
        result += line.split(";")[0]

    result += "\r\n"
    return result


EOF
