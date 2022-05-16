_BORING = {
    '/rss.xml'    ,
    '/atom.xml'   ,
    '/favicon.ico',
    '/robots.txt' ,
    '/apple-touch-icon.png',
    '/apple-touch-icon-precomposed.png',
    '/exobrain/searchindex.json',
    # todo UGH. get rid of this font..
    '/exobrain/FontAwesome/fonts/fontawesome-webfont.woff2?v=4.7.0',
    # todo ugh
    '/notset',
}


WHERE_NOT_BORING = 'uri NOT IN (' + ', '.join(f'"{x}"' for x in _BORING) + ')' + '''
AND uri NOT LIKE '%/css/%'
AND uri NOT LIKE '%.css'
AND uri NOT LIKE '%.js'
AND uri NOT LIKE '/comments/%'
AND uri NOT LIKE '%.svg'
AND uri NOT LIKE '%robot-face%'
AND referer NOT LIKE 'https://beepb00p.xyz%'
AND referer NOT LIKE '%.baidu.com/'
AND referer NOT LIKE '%www.google.%'
AND referer NOT LIKE '%duckduckgo.com/'
AND referer NOT LIKE '%instapaper.com/'
'''
