// todo maybe move this to document.js?
// NOTE: PATH_TO_ROOT is set externally

const create = x => document.createElement(x)

const load = script => new Promise((resolve, reject) => {
    const el = create('script')
    el.src = script
    document.head.appendChild(el)
    el.addEventListener('load', () => resolve(script))
})

const _makeindex = async () => {
    await load('https://unpkg.com/lunr/lunr.js')
    await load(PATH_TO_ROOT + 'documents.js')

    const key2doc = new Map()
    const idx = lunr(function () {
        this.ref('key')
        this.field('text')

        this.metadataWhitelist = ['position']

        documents.forEach((doc) => {
            doc.key = doc.file + '#' + doc.id
            key2doc.set(doc.key, doc)
            this.add(doc)
        }, this)
    })
    return [idx, key2doc]
}

const makeindex = async () => {
    if (window.cached_idx == null) {
        window.cached_idx = await _makeindex()
    }
    return window.cached_idx
}

const dosearch = async () => {
    const results = document.getElementById('search-results')
    const status  = document.getElementById('search-status')
    const query   = document.getElementById('search-query')

    const q = query.value
    const [idx, key2doc] = await makeindex()

    const sres = q == '' ? [] : idx.search(q)
    if (q == '' || sres.length > 0) {
        status.textContent = q == '' ? '' : `${sres.length} results`
        status.style.color = 'initial'
        results.textContent = '' // clear old results
    } else {
        status.textContent = `No results for: ${q}`
        status.style.color = 'red'
        // keeping old results
    }

    for (const r of sres) {
        const d = key2doc.get(r.ref)
        const link = create('a')
        link.target= '_blank'
        link.classList.add('search-result-link')
        // TODO highlight the heading matched by id by exobrain css? maybe with animation or smth
        link.href = `${PATH_TO_ROOT}${d.file}#${d.id}`
        link.textContent = d.file

        const text = d.text
        const etext = create('span')
        etext.classList.add('search-result-body')
        for ([k, v] of Object.entries(r.matchData.metadata)) {
            let cpos = 0
            // add 'fake' highlight to make code more uniform
            for (const [pos, len] of [...v.text.position, [text.length, 0]]) {
                if (pos > cpos) {
                    etext.appendChild(document.createTextNode(text.substring(cpos, pos)))
                }
                if (len == 0) {
                    break
                }
                const npos = pos + len
                const mark = create('mark')
                mark.textContent = text.substring(pos, npos)
                etext.appendChild(mark)
                cpos = npos
            }
        }

        const el   = create('div')
        el.classList.add('search-result')
        el.appendChild(link)
        el.appendChild(etext)
        results.appendChild(el)
    }
}

document.addEventListener('DOMContentLoaded', () => {
    /* todo could debounce but whatever */
    document.getElementById('search-query').addEventListener('input', dosearch)
})
