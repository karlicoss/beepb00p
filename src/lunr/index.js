// https://lunrjs.com/guides/index_prebuilding.html

// TODO put raw html here?
const dmap = new Map()
const idx = lunr(function () {
  this.ref('key')
  this.field('text')

  this.metadataWhitelist = ['position']

  documents.forEach((doc) => {
      doc.key = doc.file + '#' + doc.id
      dmap.set(doc.key, doc)
      this.add(doc)
  }, this)
})


const create = x => document.createElement(x)

const ready = () => {
    const button  = document.getElementById('search')
    const query   = document.getElementById('query')
    const results = document.getElementById('results')

    const dosearch = () => {
        const q = query.value
        const sres = idx.search(q)

        results.textContent = ''
        for (const r of sres) {
            const d = dmap.get(r.ref)

            const etext = create('span')
            etext.style.display = 'block'
            const text = d.text
            for ([k, v] of Object.entries(r.matchData.metadata)) {
                const poss = v.text.position
                let cpos = 0
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
            // todo use css
            const el   = create('div')
            el.style.paddingBottom = '0.5em'
            const link = create('a')
            link.href = `https://beepb00p.xyz/exobrain2/${d.file}#${d.id}`
            // todo don't need # in text?
            link.textContent = d.file
            el.appendChild(link)
            el.appendChild(etext)
            results.appendChild(el)
        }
    }

    /* todo could debounce but whatever */
    query.addEventListener('input', dosearch)
    search.addEventListener('click', dosearch)
}

document.addEventListener('DOMContentLoaded', ready)
