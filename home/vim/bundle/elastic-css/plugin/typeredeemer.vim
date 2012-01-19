" =============================================================================
" File:          px2em.vim
" Maintainer:    Luis Gonzalez <kuroi_kenshi96 at yahoo dot com>
" Description:   Converts font-size and line-height declarations to pixels,
"                em's, or percentages
" Last Modified: July 30, 2010
" License:       This program is free software. It comes without any warranty,
"                to the extent permitted by applicable law. You can redistribute
"                it and/or modify it under the terms of the Do What The Fuck You
"                Want To Public License, Version 2, as published by Sam Hocevar.
"                See http://sam.zoy.org/wtfpl/COPYING for more details.
" =============================================================================

" Script init stuff {{{1
if exists("g:loaded_TypeRedeemer") || &cp
    finish
endif

let g:TypeRedeemer_version = "0.1.0"
let g:TypeRedeemerCreateMappings = 1
let g:loaded_TypeReedemeer = 1
let g:parentFontSizeInPx = 16.0


" Mappings {{{1
function! s:CreateMappings(target, mapping)
    if !hasmapto(a:mapping, 'n')
        exec 'nmap' . a:mapping . ' ' . a:target
    endif
endfunction


" Create Mappings {{{2
if g:TypeRedeemerCreateMappings
    call s:CreateMappings('<Plug>TypeRedeemerToPx',                '<leader>px')
    call s:CreateMappings('<Plug>TypeRedeemerAllToPx',             '<leader>apx')
    call s:CreateMappings('<Plug>TypeRedeemerToEm',                '<leader>em')
    call s:CreateMappings('<Plug>TypeRedeemerAllToEm',             '<leader>aem')
    call s:CreateMappings('<Plug>TypeRedeemerToPercent',           '<leader>p%')
    call s:CreateMappings('<Plug>TypeRedeemerAllToPercent',        '<leader>ap%')
    call s:CreateMappings('<Plug>TypeRedeemerSetDefaultFontSize',  '<leader>sd')
    call s:CreateMappings('<Plug>TypeRedeemerShowDefaultFontSize', '<leader>gd')

    let g:TypeRedeemerShowDefaultSize = 0
endif


" Define Mappings {{{2
noremap <silent> <script> <Plug>TypeRedeemerToPx                :call TypeRedeemerConvert('px')<CR>
noremap <silent> <script> <Plug>TypeRedeemerAllToPx             :call TypeRedeemerConvert('px', 'all')<CR>
noremap <silent> <script> <Plug>TypeRedeemerToEm                :call TypeRedeemerConvert('em')<CR>
noremap <silent> <script> <Plug>TypeRedeemerAllToEm             :call TypeRedeemerConvert('em', 'all')<CR>
noremap <silent> <script> <Plug>TypeRedeemerToPercent           :call TypeRedeemerConvert('percent')<CR>
noremap <silent> <script> <Plug>TypeRedeemerAllToPercent        :call TypeRedeemerConvert('percent', 'all')<CR>
noremap <silent> <script> <Plug>TypeRedeemerSetDefaultFontSize  :call SetParentFontSize()<CR>
noremap <silent> <script> <Plug>TypeRedeemerShowDefaultFontSize :call GetParentFontSize()<CR>



" Wrapper Conversion Method {{{1
" -----------------------------------------------------------------------------
"  TypeRedeemerConvert: Delegates unit conversion to SelectRegex
"  Args:
"    unit:  'percent', 'px', or 'em'
"    {all}: specify 'all' to convert all font-size/line-height declarations is
"           a stylesheet to the given unit
function! TypeRedeemerConvert(unit, ...)
    return a:0 >= 1 ? s:SelectRegex(a:unit, a:1) : s:SelectRegex(a:unit)
endfunction



" Conversion Formulas {{{1
" -----------------------------------------------------------------------------
"  EmEquivalent: Convert value to em value {{{2
"  Args:
"    val: the value to convert, i.e. '12px' or '75%'
function! s:EmEquivalent(val)
    let value = s:GetNumericValue(a:val)
    let type = s:GetCurrentType(a:val)

    if type != 'em'
        if type == 'px'
            return s:FloatOrWholeNumber(value/g:parentFontSizeInPx)
        elseif type == 'percent'
            return s:FloatOrWholeNumber(value/100.0)
        endif
    else
        return s:FloatOrWholeNumber(value)
    endif
endfunction


" -----------------------------------------------------------------------------
"  PxEquivalent: Convert value to pixel value {{{2
"  Args:
"    val: the value to convert, i.e. '2em' or '75%'
function! s:PxEquivalent(val)
    let value = s:GetNumericValue(a:val)
    let type = s:GetCurrentType(a:val)

    if type != 'px'
        if type == 'em'
            return s:FloatOrWholeNumber(value * g:parentFontSizeInPx)
        elseif type == 'percent'
            return s:FloatOrWholeNumber(value * (g:parentFontSizeInPx/100.0))
        endif
    else
        return s:FloatOrWholeNumber(value)
    endif
endfunction


" -----------------------------------------------------------------------------
"  PercentEquivalent: Convert value to percent value {{{2
"  Args:
"    val: the value to convert, i.e. '12px' or '0.5em'
function! s:PercentEquivalent(val)
    let value = s:GetNumericValue(a:val)
    let type = s:GetCurrentType(a:val)

    if type != 'percent'
        if type == 'px'
            return s:FloatOrWholeNumber((value/g:parentFontSizeInPx) * 100)
        elseif type == 'em'
            return s:FloatOrWholeNumber(value * 100)
        endif
    else
        return s:FloatOrWholeNumber(value)
    endif
endfunction


" Helper Functions {{{1
" -----------------------------------------------------------------------------
"  GetCurrentType: Decipher what unit of measurement we're converting from {{{2
"  Args:
"    val: value we're converting from, i.e. '12px', '0.75em', '100%'
function! s:GetCurrentType(val)
    let type = matchstr(a:val, '\vpx|em|\%')
    if type == '%'
        return 'percent'
    elseif type == 'px' || type == 'em'
        return type
    endif
endfunction


" -----------------------------------------------------------------------------
"  GetNumericValue: Retrieve the numeric portion of the unit we're converting {{{2
"                   from
"  Args:
"    val: value we're converting from, i.e. '12px', '0.75em', '100%'
function! s:GetNumericValue(val)
    return str2float(matchstr(a:val, '\v\d+(\.\d{1,6})?'))
endfunction


" -----------------------------------------------------------------------------
"  SelectRegex: Figures out which sustitution regex to use {{{2
"  Args:
"    unit:  unit we're converting from - 'percent', 'px', or 'em'
"    {all}: specify 'all' to convert all font-size/line-height declarations is
"           a stylesheet to the given unit
function! s:SelectRegex(unit, ...)
    let rx = 's/\v((font-size|line-height): )(\d+(\.\d{1,6})?(px|em|\%));/\=submatch(1).string('
    if a:unit == 'percent'
        let rx = rx . 's:PercentEquivalent(submatch(3)))."%;"/'
    elseif a:unit == 'px'
        let rx = rx . 's:PxEquivalent(submatch(3)))."px;"/'
    elseif a:unit == 'em'
        let rx = rx . 's:EmEquivalent(submatch(3)))."em;"/'
    endif
    return a:0 >= 1 ? s:RunConversion(rx, a:1) : s:RunConversion(rx)
endfunction


" -----------------------------------------------------------------------------
"  RunConversion: Runs the appropriate substitute command to convert from one {{{2
"                 unit to another
"  Args:
"    rx:    the regular expression to use
"    {all}: specify 'all' to convert all font-size/line-height declarations is
"           a stylesheet to the given unit
function! s:RunConversion(rx, ...)
    let all = a:0 >= 1 ? a:1 : ''
    exe all == 'all' ? 'silent! ' . '%' . a:rx : 'silent! ' . a:rx
endfunction


" -----------------------------------------------------------------------------
"  FloatOrWholeNumber: Returns a whole number if the value passed in ends in {{{2
"                      '.0', a float otherwise
"  Args:
"    val: value we're converting from, i.e. '12px', '0.75em', '100%'
function! s:FloatOrWholeNumber(val)
    return matchstr(string(a:val), '\v\.(0{1,})$') == '.0' ? float2nr(a:val) : a:val
endfunction


" -----------------------------------------------------------------------------
"  SetParentFontSize: Prompt the user to enter a new default parent font size {{{2
function! SetParentFontSize()
    call inputsave()
    let input = input('New default parent font size in px: ') 
    call inputrestore()

    if input =~ '\v\d+\.\d+'
        let g:parentFontSizeInPx = str2float(input)
    else
        throw "Font size must be a float."
    endif
endfunction


" -----------------------------------------------------------------------------
"  GetParentFontSize: Retrieve the current value of the default parent font size {{{2
function! GetParentFontSize()
    echo 'Current default font size: ' . string(g:parentFontSizeInPx)
endfunction


" vim:ft=vim foldmethod=marker sw=4
