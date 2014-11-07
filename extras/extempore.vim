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
"     permanent! (:so % to source current file)
"  Option 2. Place extempore.vim in your vim plugin directory.
"
" TODO:
"   - allow vim command to specify host and port
"   - speed up algorithm for finding enclosing block
"   - add an indicator that a command was sent, maybe an underline or flash
"   - clean up code a bit

" Use Scheme syntax highlighting
au BufNewFile,BufRead *.xtm set filetype=scheme

" Define all of the Extempore commands
command! -nargs=* ExtemporeOpenConnection :python connect()
command! -nargs=* ExtemporeOutputPoller :python output_poller()
command! -nargs=* ExtemporeCloseConnection :python close()
command! -nargs=* ExtemporePanic :python panic()
command! -nargs=* ExtemporeSendEnclosingBlock :python send_enclosing_block()
command! -nargs=* ExtemporeSendEntireFile :python send_entire_file()
command! -nargs=* ExtemporeSendSelection :python send_selection()
command! -nargs=* ExtemporeSendBracketSelection :python send_bracket_selection()
command! -nargs=* ExtemporeSendUserInput :python send_user_input()

"" Add this (minus leading ") to netrwFileHandlers.vim
"" ---------------------------------------------------------------------
"" s:NFH_html: handles extempore when the user hits "x" when the {{{1
""                        cursor is atop a *.xtm file
"fun! s:NFH_xtm(xtmfile)
""  call Dfunc("s:NFH_xtm(".a:xtmfile.")")
""  call Decho("executing !mozilla ".a:xtmfile)
"  exe "!send_file -i ".shellescape(a:xtmfile,1)
""  call Dret("s:NFH_xtm 1")
"  return 1
"endfun

" These are the mappings I found to be convenient, which
" didn't interfere with my own personal setup. Feel free to change them.
nnoremap <Leader>o :ExtemporeOpenConnection() <CR>
nnoremap <Leader>O :ExtemporeCloseConnection() <CR>
nnoremap <Leader>x :ExtemporeCloseConnection() <CR>
nnoremap <Leader>w :ExtemporeSendEnclosingBlock() <CR>
nnoremap <Leader>a :ExtemporeSendEntireFile() <CR>
nnoremap <Leader>s :ExtemporeSendSelection() <CR>

nmap <F12> :ExtemporeSendUserInput()<CR>

nmap <Bar> :ExtemporeSendEntireFile()<CR>
nmap <BS> :ExtemporePanic()<CR>

nmap ] :ExtemporeSendBracketSelection()<CR>

nmap <Return> :ExtemporeSendSelection()<CR>
xmap <Return> <C-c> :ExtemporeSendSelection()<CR>

nmap <Tab> :ExtemporeSendEnclosingBlock()<CR>
nmap <S-Tab> :ExtemporeSendEnclosingBlock()<CR>
xmap <S-Tab> <C-c>:ExtemporeSendEnclosingBlock()<CR>
imap <S-Tab> <Esc>:ExtemporeSendEnclosingBlock()<CR>

python << EOF
import vim
import telnetlib
import threading

HOST = "localhost"
PORT = 7099

telnet = None

def read_output():
  global telnet
  to_return = ""
  if not telnet:
    print "Not connected"
    return to_return
  try:
    to_return = telnet.read_eager()
  except:
    print "Error reading from extempore connection"
    telnet = None
  return to_return

def output_poller():
  global telnet
  if not telnet:
    return
  output = read_output()
  if output != "":
    print output
  threading.Timer(0.3, output_poller).start()

def connect():
    """ Opens the connection """
    global telnet
    telnet = telnetlib.Telnet(HOST, PORT)
    output_poller()

def close():
    global telnet
    if telnet:
        telnet.close()
        telnet = None

def send_string(value):
    """ Sends the desired string through the connection """
    global telnet
    if not telnet:
        print "Not connected"
        return
    if value:
        telnet.write(value)

def get_user_input():
    vim.command('call inputsave()')
    vim.command("let user_input = input('extempore>: ')")
    vim.command('call inputrestore()')
    user_input = vim.eval('user_input')
    return user_input

def echo_user_input():
    print get_user_input()

def send_user_input():
    send_string(get_user_input()+"\r\n")

def panic():
    send_string("(bind-func dsp (lambda (in:SAMPLE time:i64 channel:SAMPLE data:SAMPLE*) 0.0))\r\n")

def send_enclosing_block():
    """ Grab the enclosing function block and send it, ie if you
        are inside a (define ...) somewhere, we want to send that."""
    send_string(get_enclosing_block())

def send_entire_file():
    send_string(get_entire_file())

def send_selection():
    """ Send the text determined by the '<' and '>' marks. """
    send_string(get_selection())

def send_bracket_selection():
    """ Send the text determined by the '[' and ']' marks. """
    send_string(get_bracket_selection())

def get_entire_file():
    lines = vim.current.buffer
    result = join_lines(lines)
    return result

def send_path_file(path):
    file_data = open(path).read()
    send_string(file_data+"\r\n")

def get_selection():
    lines = vim.current.buffer
    start_selection, col = vim.current.buffer.mark("<")
    # vim index is not 0 based, facepalm.jpg
    start_selection -= 1
    end_selection, col = vim.current.buffer.mark(">")

    result = join_lines(lines[start_selection:end_selection])

    return result

def get_bracket_selection():
    lines = vim.current.buffer
    start_selection, col = vim.current.buffer.mark("[")
    # vim index is not 0 based, facepalm.jpg
    start_selection -= 1
    end_selection, col = vim.current.buffer.mark("]")

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
