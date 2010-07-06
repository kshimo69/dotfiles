" clmemogrep.vim -- ChangeLog�����֘A�c�[��
" 
" version : 0.0.3
" author : ampmmn(htmnymgw <delete>@<delete> gmail.com)
" url    : http://d.hatena.ne.jp/ampmmn
"
" ----
" history
"	 0.0.3		2009-03-04	minor changes.
"	 0.0.2		2009-02-23	add folding and header.
"	 0.0.1		2009-02-19	initial release.
" ----


if exists('loaded_clmemogrep') || &cp
  finish
endif
let loaded_clmemogrep=1

" Check Env.
if !has('python')"{{{
	echoerr "Required Vim compiled with +python"
	finish
endif
if v:version < 700
	echoerr "clmemogrep.vim requires Vim 7.0 or later."
	finish
endif"}}}


" Global Variables"{{{

" ChnageLog�����t�@�C���̃p�X
if exists("g:clmemogrep_changelogfilepath") == 0
	let g:clmemogrep_changelogfilepath = './ChangeLog'
endif

" ChnageLog�����t�@�C���̕����R�[�h
if exists("g:clmemogrep_fileencoding") == 0
	let g:clmemogrep_fileencoding = &enc
endif

" item�̊J�n�p�^�[��(Python���K�\��)
if exists("g:clmemogrep_startpattern") == 0
	let g:clmemogrep_startpattern = '^\s\*'
	" �f�t�H���g�ݒ�́A1�J�����ڂ��󔒂�2�J�����ڂ�asterisk�Ŏn�܂�s
endif
" item�̏I�[�p�^�[��(Python���K�\��)
if exists("g:clmemogrep_endpatern") == 0
	let g:clmemogrep_endpatern = '^($|\S)'
	" �f�t�H���g�ݒ�́A��s�܂���1�J�����ڂ���n�܂�s(=entry header)
endif

" �o�͐�E�C���h�E����������ۂ̃p�^�[��
if exists("g:clmemogrep_outputwindow") == 0
	let g:clmemogrep_outputwindow = '^__ChangeLogMemoGrep$'
endif

" �o�̓E�C���h�E�̈ʒu
if exists("g:clmemogrep_Direction") == 0
	let g:clmemogrep_Direction = "rightbelow"
endif
if exists("g:clmemogrep_Split") == 0
	let g:clmemogrep_Split = ''
	"let g:clmemogrep_Split = 'v' �Ő�������
endif

" �o�̓E�C���h�E�̍���
if exists("g:clmemogrep_WindowHeight") == 0
	let g:clmemogrep_WindowHeight = 15
	" 0���w�肵���ꍇ�A���݂̃E�C���h�E�T�C�Y�̔����ɕ������܂�
endif

" �������s��A�o�̓E�C���h�E�ɃJ�[�\�����ړ����邩?(0�Ō��̏�Ԃ��ێ�)
if exists("g:clmemogrep_setfocus") == 0
	let g:clmemogrep_setfocus=1
endif

" �܂肽���ݕ\���ɂ���
if exists("g:clmemogrep_fold") == 0
	let g:clmemogrep_fold=0
endif

" �w�b�_��\��
if exists("g:clmemogrep_showheader") == 0
	let g:clmemogrep_showheader=1
endif

" ���t��\��
if exists("g:clmemogrep_showdate") == 0
	let g:clmemogrep_showdate=0
endif

" �A�C�e���Ԃ���؂镶����(����`�̏ꍇ�͊Ԃ�u�����ɏo��)
"let g:clmemogrep_itemseparator="----------"


"}}} Global Variables

" Functions
let s:clmemogrep_init=0
function! s:python_part_init()"{{{
	" ����̂ݎ��s
	if s:clmemogrep_init != 0
		return
	endif
python << END_OF_PYTHON_PART


def clmemo_search(filePath):
	import vim,re

	fenc          = vim.eval("g:clmemogrep_fileencoding")
	enc           = vim.eval("&enc")
	end_pattern   = re.compile(vim.eval("g:clmemogrep_endpatern").decode(enc))
	start_pattern = re.compile(vim.eval("g:clmemogrep_startpattern").decode(enc))
	date_pattern  = re.compile(r'^(\d+?)-(\d+?)-(\d+)'.decode(enc))

	def insertIf(_):
		_ = _.replace(u'\\', u'\\\\').replace(u'"', ur'\"').replace(u'\n', ur'\n')
		cmdline= u'_result.insertIf("%s",%s,%s,%s)' % (_, year,month,day)
		vim.eval(cmdline.encode(enc))
	
	year,month,day='0','0','0'
	cur_item=''
	# ��ʂ̑��Ńt�@�C���p�X�`�F�b�N�͊������Ă�����̂Ƃ���
	for line in open(filePath):
		line = line.decode(fenc)
		# �w�b�_�̏����ɍ��v����ꍇ�͓��t���X�V
		if end_pattern.match(line):
			insertIf(cur_item)
			cur_item = ''
			if date_pattern.match(line):
				year,month,day = date_pattern.match(line).groups()
			continue
		# �A�C�e���w�b�_�ɍ��v����ꍇ�̓A�C�e���P�ʂł̎��W���J�n
		if start_pattern.match(line):
			insertIf(cur_item)
			cur_item = line.lstrip()
			continue
		cur_item += line.lstrip()

END_OF_PYTHON_PART
	let s:clmemogrep_init = 1
endfunction"}}}

let s:tmpl = { "patterns":[], "dates":[], "results":[], "exclude":[] }

" patterns�ɂ��ׂă}�b�`����Ȃ當�����ێ�
function! s:tmpl.insertIf(expr,year,month,day)"{{{

	if len(a:expr)==0 | return | endif

	for pat in self.patterns
		if a:expr !~ pat
			return 0
		endif
	endfor
	" ���O�p�^�[���Ɋ܂܂��A�C�e���͌��ʂɊ܂܂Ȃ�
	for pat in self.exclude
		if a:expr =~ pat
			return 0
		endif
	endfor

	" dates�ɗv�f�����݂���ꍇ�́A���̗v�f���������t�Ɉ�v����A�C�e���݂̂�ǉ�����
	if len(self.dates) > 0
		for date in self.dates
			if a:year != date[0] || a:month != date[1] || a:day != date[2]
				continue
			endif
			let self.results += [ [a:expr,[a:year,a:month,a:day] ] ]
			return 1
			break
		endfor
		return 0
	else
		let self.results += [ [a:expr,[a:year,a:month,a:day] ] ]
		return 1
	endif
endfunction"}}}

function! s:search(filepath, keywords,exclude,dates)"{{{
	if filereadable(a:filepath) == 0
		echohl ErrorMsg
		echo a:filepath . ' : Changelog memo is not filereadable.'
		echohl
		return []
	endif
	let _result          = deepcopy(s:tmpl)
	let _result.patterns = a:keywords
	let _result.exclude  = a:exclude
	let _result.dates    = a:dates

	call s:python_part_init()
	python import vim
	python clmemo_search(vim.eval("a:filepath"))

	if len(_result.results) == 0
		echohl WarningMsg
		echo "Item not found."
		echohl
		return []
	endif

	return _result.results
endfunction"}}}

" ��������
" filepath:Changelog�����t�@�C���̃p�X
" reverse: ���ʂ̔��](��0�Ŕ��])
" a:000�L�[���[�h�̃��X�g(AND����)
function! s:grep(filepath, reverse, ...)"{{{
	if len(a:000)==0
		return
	endif
	let results = s:search(a:filepath, a:000, [], [])
	if len(results) == 0
		return
	endif

	" �J�����_�[�\���̍X�V
	call s:setCalendarSignFunction(results, 0, 0)


	if a:reverse
		call reverse(results)
	endif

	call s:print(results,g:clmemogrep_showheader, a:000)

endfunction "}}}

" �o�̓o�b�t�@ & �E�C���h�E�̍쐬
function! s:open()"{{{

	let bname = '__ChangeLogMemoGrep'
	let cur_winnr = winnr()

	let win_height = g:clmemogrep_WindowHeight
	if g:clmemogrep_WindowHeight==0
		let win_height=""
	endif


	" �o�b�t�@�����݂��Ȃ���΁A�o�̓E�C���h�E�ƂƂ��ɍ쐬
	if bufexists(bname) == 0
		silent execute printf('%s %s %snew', g:clmemogrep_Direction, win_height, g:clmemogrep_Split)
		setlocal bufhidden=unload
		setlocal nobuflisted
		setlocal buftype=nofile
		setlocal nomodifiable
		setlocal noswapfile
"		setlocal nonumber
		setlocal filetype=clmemogrep
		setlocal laststatus=0
		silent file `=bname`
	else
		" �o�b�t�@�̓E�C���h�E��ɕ\������Ă��邩? �Ȃ���΃E�C���h�E�����쐬
		let nr = bufnr(g:clmemogrep_outputwindow)
		let winnr = bufwinnr(nr)
		if winnr != -1
			return winnr
		endif

		execute printf('%s %d %ssplit',g:clmemogrep_Direction, win_height, g:clmemogrep_Split)
		silent execute nr 'buffer'
	endif

	execute cur_winnr 'wincmd w'

	return bufwinnr(g:clmemogrep_outputwindow)
endfunction "}}}

" �������ʃf�[�^���o�̓E�C���h�E��ɕ\��
function! s:print(results,showheader, keywords)"{{{
	let [output_winnr, cur_winnr] = [s:open(), winnr()]

	execute  output_winnr 'wincmd w'

	"�����̓��e��S�폜���A�V�������e�ɒu��������
	setlocal modifiable
	silent! execute 1 'delete _' line('$')

	" �w�b�_�\��
	if a:showheader
			silent! call append(0, printf("Results %d items for %s", len(a:results), join(a:keywords,' ')))
	endif

	for resultitem in a:results
		let [item, date] = resultitem
		let item = substitute(item, '\r', '\n', 'g')
		for _ in split(item, '\n')
			if g:clmemogrep_showdate!=0
				let _ = substitute(_, '^\*\s', printf('*[%04d-%02d-%02d] ', date[0],date[1],date[2]), '')
			endif
			silent! call append(line('$')-1, _)
		endfor
		if exists("g:clmemogrep_itemseparator")
			silent! call append(line('$')-1, g:clmemogrep_itemseparator)
		endif
	endfor
	normal! 1G
	setlocal nomodifiable

	" �V���^�b�N�X�Ɛ܂肽���݂̐ݒ�
	call s:setupSyntaxKeywordsAndFold(a:keywords)

	" �K�v�ɉ����ďo�̓E�C���h�E�ɃJ�[�\�����ړ�
	if g:clmemogrep_setfocus == 0
		execute cur_winnr 'wincmd w'
	endif
endfunction"}}}

function! s:setupSyntaxKeywordsAndFold(keywords)"{{{
	" �����L�[���[�h���݂̂������\������V���^�b�N�X�̐ݒ�
	silent! execute "syn clear CLMGKeyword"
	for _ in a:keywords
		silent! execute "syn match CLMGKeyword `" . _ . "`"
	endfor
	silent! execute "hi! link CLMGKeyword Keyword"
	" �A�C�e���ԃZ�p���[�^���ݒ肳��Ă���ꍇ���V���^�b�N�X��ݒ�
	silent! execute "syn clear CLMGComment"
	if exists("g:clmemogrep_itemseparator") && len(g:clmemogrep_itemseparator) > 0
		silent! execute "syn match CLMGComment `^" . g:clmemogrep_itemseparator . "$`"
		silent! execute "hi! link CLMGComment Comment"
	endif
	" �܂肽���݂̐ݒ�
	silent! syn clear clmemogrepItem
	if g:clmemogrep_fold != 0
		syn region clmemogrepItem start="^\*" end="\n^\*" fold transparent excludenl
		setl foldmethod=syntax
	endif
	" �܂肽���ݓW�J���ɁA�E�C���h�E���ɂ��ׂĎ��܂�ꍇ�͐܂肽���܂Ȃ�
	if line("$") <= winheight(0)
		setlocal modifiable
		silent normal 1GVGGzO
		setlocal nomodifiable
	endif
endfunction"}}}

" �w�肵���N�����Ɉ�v����G���g���w�b�_�ʒu���擾
function s:getpos(year,month,day)"{{{
	let pos = getpos('.')
	
	" �o�b�t�@��ΏۂɌ���
	let expr = printf('^%04d-%02d-%02d', a:year, a:month, a:day)

	let searchpos = search(expr, 'w')
	if searchpos == 0
		call setpos('.', pos)
		return [0,0,0,0]
	endif

	let entrypos = getpos('.')
	call setpos('.', pos)
	return entrypos
endfunction"}}}

" calendar_sign�t�b�N�֐��ƃJ�����_�[�\���̍X�V
function! s:setCalendarSignFunction(results, createwindow, setfocus)"{{{
	let win_nr = winnr()

	" FIXME:�d��������t��if���蕶����������Ă��܂�
	let signFunc="function! g:clmemogrep_CalendarSign(day,month,year)\n"
	for _ in a:results
		let [item, date] = _
		let signFunc.= printf("if a:year==%d && a:month==%d && a:day == %d|return '@'|endif\n", date[0], date[1], date[2])
	endfor
	let signFunc.="return 0\n"
	let signFunc.="endfunction"
	exe signFunc
	let g:calendar_sign="g:clmemogrep_CalendarSign"

	" �J�����_�[�E�C���h�E�̍ĕ\��
	if exists(":Calendar") == 2 && exists(":CalendarH") == 2
		let nr = bufnr('__Calendar')
		" a:createwindow�l��0�̏ꍇ�A�J�����_�[�o�b�t�@�����łȂ���΍X�V���Ȃ�
		if a:createwindow == 0 && bufwinnr(nr) == -1
			return
		endif
		if nr == -1
			Calendar
		else
			let dir = getbufvar(nr, 'CalendarDir')
			exe dir==0? "Calendar": "CalendarH"
		endif
	endif

	if a:setfocus==0
		execute  win_nr 'wincmd w'
	endif
endfunction"}}}

" �w�肵���L�[���[�h���܂ރA�C�e���̓��t��calendar���Sign�\��
function! s:calendarSign(filepath, createwindow, ...)"{{{
	if exists('g:calendar_version') == 0
		echohl WarningMsg
		echo 'calendar.vim not exists.'
		echohl
		return
	endif
	let results = s:search(a:filepath, a:000, [], [])
	if len(results) == 0
		return
	endif
	return s:setCalendarSignFunction(results, a:createwindow, 1)
endfunction
"}}}

" calendar.vim�̃A�N�V�����t�b�N�p�֐�
" ChangeLog������̎w����t�̃G���g���w�b�_�ɃW�����v
function! CalendarActionCLMemo(day, month, year, week, dir)"{{{
	let cal_winnr = bufwinnr(bufnr('__Calendar'))

	" ChangeLog������\�����Ă���E�C���h�E���Ȃ���΁A�E�C���h�E���쐬
	" �J�����_�[�̌����ɍ��킹�āA�J�����_�[�ɗאڂ���`�ō쐬���܂��B
	let nr = bufnr(g:clmemogrep_changelogfilepath)
	let cur_nr = bufwinnr(nr)
	if cur_nr == -1
		if a:dir == 'H'
			execute "normal! \<c-w>k"
			let cur_nr = winnr()
			if cur_nr == cal_winnr
				execute "normal! \<c-w>j"
				let cur_nr = winnr()
				if cur_nr == cal_winnr
					new
					let cur_nr = winnr()
				endif
			endif
		else
			execute "normal! \<c-w>l"
			let cur_nr = winnr()
			if cur_nr == cal_winnr
				execute "normal! \<c-w>h"
				let cur_nr = winnr()
				if cur_nr == cal_winnr
					rightbelow vnew
					let cur_nr = winnr()
				endif
			endif
		endif
		execute "edit" g:clmemogrep_changelogfilepath
		let nr = bufnr(".")
	endif
	execute cur_nr 'wincmd w'
	silent execute nr 'buffer'

	" ChangeLog�����̎w����t�ɃW�����v
	let entry_pos = s:getpos(a:year,a:month,a:day)
	if entry_pos == [0,0,0,0]
		return
	endif
	call setpos(".", entry_pos)
	execute "normal! z\<cr>"
endfunction"}}}


" Commands"{{{

" ����
command! -nargs=* CLMemoGrep call <SID>grep(expand(g:clmemogrep_changelogfilepath), 0, <f-args>)
" ���� �t���\��
command! -nargs=* CLMemoGrepReverse call <SID>grep(expand(g:clmemogrep_changelogfilepath), 1, <f-args>)

" �w�肵���L�[���[�h���܂ރG���g���̓��t���J�����_�[��ɕ\��
command -nargs=* CLMemoCalendarSign call <SID>calendarSign(expand(g:clmemogrep_changelogfilepath),1, <f-args>)

"}}}


" vim:foldmethod=marker

