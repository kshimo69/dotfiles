" clmemogrep.vim -- ChangeLogメモ関連ツール
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

" ChnageLogメモファイルのパス
if exists("g:clmemogrep_changelogfilepath") == 0
	let g:clmemogrep_changelogfilepath = './ChangeLog'
endif

" ChnageLogメモファイルの文字コード
if exists("g:clmemogrep_fileencoding") == 0
	let g:clmemogrep_fileencoding = &enc
endif

" itemの開始パターン(Python正規表現)
if exists("g:clmemogrep_startpattern") == 0
	let g:clmemogrep_startpattern = '^\s\*'
	" デフォルト設定は、1カラム目が空白で2カラム目がasteriskで始まる行
endif
" itemの終端パターン(Python正規表現)
if exists("g:clmemogrep_endpatern") == 0
	let g:clmemogrep_endpatern = '^($|\S)'
	" デフォルト設定は、空行または1カラム目から始まる行(=entry header)
endif

" 出力先ウインドウを検索する際のパターン
if exists("g:clmemogrep_outputwindow") == 0
	let g:clmemogrep_outputwindow = '^__ChangeLogMemoGrep$'
endif

" 出力ウインドウの位置
if exists("g:clmemogrep_Direction") == 0
	let g:clmemogrep_Direction = "rightbelow"
endif
if exists("g:clmemogrep_Split") == 0
	let g:clmemogrep_Split = ''
	"let g:clmemogrep_Split = 'v' で垂直分割
endif

" 出力ウインドウの高さ
if exists("g:clmemogrep_WindowHeight") == 0
	let g:clmemogrep_WindowHeight = 15
	" 0を指定した場合、現在のウインドウサイズの半分に分割します
endif

" 検索実行後、出力ウインドウにカーソルを移動するか?(0で元の状態を維持)
if exists("g:clmemogrep_setfocus") == 0
	let g:clmemogrep_setfocus=1
endif

" 折りたたみ表示にする
if exists("g:clmemogrep_fold") == 0
	let g:clmemogrep_fold=0
endif

" ヘッダを表示
if exists("g:clmemogrep_showheader") == 0
	let g:clmemogrep_showheader=1
endif

" 日付を表示
if exists("g:clmemogrep_showdate") == 0
	let g:clmemogrep_showdate=0
endif

" アイテム間を句切る文字列(未定義の場合は間を置かずに出力)
"let g:clmemogrep_itemseparator="----------"


"}}} Global Variables

" Functions
let s:clmemogrep_init=0
function! s:python_part_init()"{{{
	" 初回のみ実行
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
	# 上位の側でファイルパスチェックは完了しているものとする
	for line in open(filePath):
		line = line.decode(fenc)
		# ヘッダの書式に合致する場合は日付を更新
		if end_pattern.match(line):
			insertIf(cur_item)
			cur_item = ''
			if date_pattern.match(line):
				year,month,day = date_pattern.match(line).groups()
			continue
		# アイテムヘッダに合致する場合はアイテム単位での収集を開始
		if start_pattern.match(line):
			insertIf(cur_item)
			cur_item = line.lstrip()
			continue
		cur_item += line.lstrip()

END_OF_PYTHON_PART
	let s:clmemogrep_init = 1
endfunction"}}}

let s:tmpl = { "patterns":[], "dates":[], "results":[], "exclude":[] }

" patternsにすべてマッチするなら文字列を保持
function! s:tmpl.insertIf(expr,year,month,day)"{{{

	if len(a:expr)==0 | return | endif

	for pat in self.patterns
		if a:expr !~ pat
			return 0
		endif
	endfor
	" 除外パターンに含まれるアイテムは結果に含まない
	for pat in self.exclude
		if a:expr =~ pat
			return 0
		endif
	endfor

	" datesに要素が存在する場合は、その要素が示す日付に一致するアイテムのみを追加する
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

" 検索処理
" filepath:Changelogメモファイルのパス
" reverse: 結果の反転(非0で反転)
" a:000キーワードのリスト(AND検索)
function! s:grep(filepath, reverse, ...)"{{{
	if len(a:000)==0
		return
	endif
	let results = s:search(a:filepath, a:000, [], [])
	if len(results) == 0
		return
	endif

	" カレンダー表示の更新
	call s:setCalendarSignFunction(results, 0, 0)


	if a:reverse
		call reverse(results)
	endif

	call s:print(results,g:clmemogrep_showheader, a:000)

endfunction "}}}

" 出力バッファ & ウインドウの作成
function! s:open()"{{{

	let bname = '__ChangeLogMemoGrep'
	let cur_winnr = winnr()

	let win_height = g:clmemogrep_WindowHeight
	if g:clmemogrep_WindowHeight==0
		let win_height=""
	endif


	" バッファが存在しなければ、出力ウインドウとともに作成
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
		" バッファはウインドウ上に表示されているか? なければウインドウだけ作成
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

" 検索結果データを出力ウインドウ上に表示
function! s:print(results,showheader, keywords)"{{{
	let [output_winnr, cur_winnr] = [s:open(), winnr()]

	execute  output_winnr 'wincmd w'

	"既存の内容を全削除し、新しい内容に置き換える
	setlocal modifiable
	silent! execute 1 'delete _' line('$')

	" ヘッダ表示
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

	" シンタックスと折りたたみの設定
	call s:setupSyntaxKeywordsAndFold(a:keywords)

	" 必要に応じて出力ウインドウにカーソルを移動
	if g:clmemogrep_setfocus == 0
		execute cur_winnr 'wincmd w'
	endif
endfunction"}}}

function! s:setupSyntaxKeywordsAndFold(keywords)"{{{
	" 検索キーワード部のみを強調表示するシンタックスの設定
	silent! execute "syn clear CLMGKeyword"
	for _ in a:keywords
		silent! execute "syn match CLMGKeyword `" . _ . "`"
	endfor
	silent! execute "hi! link CLMGKeyword Keyword"
	" アイテム間セパレータが設定されている場合をシンタックスを設定
	silent! execute "syn clear CLMGComment"
	if exists("g:clmemogrep_itemseparator") && len(g:clmemogrep_itemseparator) > 0
		silent! execute "syn match CLMGComment `^" . g:clmemogrep_itemseparator . "$`"
		silent! execute "hi! link CLMGComment Comment"
	endif
	" 折りたたみの設定
	silent! syn clear clmemogrepItem
	if g:clmemogrep_fold != 0
		syn region clmemogrepItem start="^\*" end="\n^\*" fold transparent excludenl
		setl foldmethod=syntax
	endif
	" 折りたたみ展開時に、ウインドウ内にすべて収まる場合は折りたたまない
	if line("$") <= winheight(0)
		setlocal modifiable
		silent normal 1GVGGzO
		setlocal nomodifiable
	endif
endfunction"}}}

" 指定した年月日に一致するエントリヘッダ位置を取得
function s:getpos(year,month,day)"{{{
	let pos = getpos('.')
	
	" バッファを対象に検索
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

" calendar_signフック関数とカレンダー表示の更新
function! s:setCalendarSignFunction(results, createwindow, setfocus)"{{{
	let win_nr = winnr()

	" FIXME:重複する日付のif判定文が生成されてしまう
	let signFunc="function! g:clmemogrep_CalendarSign(day,month,year)\n"
	for _ in a:results
		let [item, date] = _
		let signFunc.= printf("if a:year==%d && a:month==%d && a:day == %d|return '@'|endif\n", date[0], date[1], date[2])
	endfor
	let signFunc.="return 0\n"
	let signFunc.="endfunction"
	exe signFunc
	let g:calendar_sign="g:clmemogrep_CalendarSign"

	" カレンダーウインドウの再表示
	if exists(":Calendar") == 2 && exists(":CalendarH") == 2
		let nr = bufnr('__Calendar')
		" a:createwindow値が0の場合、カレンダーバッファが可視でなければ更新しない
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

" 指定したキーワードを含むアイテムの日付をcalendar上にSign表示
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

" calendar.vimのアクションフック用関数
" ChangeLogメモ上の指定日付のエントリヘッダにジャンプ
function! CalendarActionCLMemo(day, month, year, week, dir)"{{{
	let cal_winnr = bufwinnr(bufnr('__Calendar'))

	" ChangeLogメモを表示しているウインドウがなければ、ウインドウを作成
	" カレンダーの向きに合わせて、カレンダーに隣接する形で作成します。
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

	" ChangeLogメモの指定日付にジャンプ
	let entry_pos = s:getpos(a:year,a:month,a:day)
	if entry_pos == [0,0,0,0]
		return
	endif
	call setpos(".", entry_pos)
	execute "normal! z\<cr>"
endfunction"}}}


" Commands"{{{

" 検索
command! -nargs=* CLMemoGrep call <SID>grep(expand(g:clmemogrep_changelogfilepath), 0, <f-args>)
" 検索 逆順表示
command! -nargs=* CLMemoGrepReverse call <SID>grep(expand(g:clmemogrep_changelogfilepath), 1, <f-args>)

" 指定したキーワードを含むエントリの日付をカレンダー上に表示
command -nargs=* CLMemoCalendarSign call <SID>calendarSign(expand(g:clmemogrep_changelogfilepath),1, <f-args>)

"}}}


" vim:foldmethod=marker

