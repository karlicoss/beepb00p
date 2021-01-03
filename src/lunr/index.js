// https://lunrjs.com/guides/index_prebuilding.html
// TODO build index from raw htmls? and just make them visible? ugh.

// TODO put raw html here?
const dmap = new Map()
const idx = lunr(function () {
  this.ref('key')
  this.field('text')

    // TODO iter map?
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

    // query.value = 'hi'

    window.query   = query
    window.results = results

    // TODO clear first?
    const dosearch = () => {
        const q = query.value
        const sres = idx.search(q)

        results.textContent = ''
        for (const r of sres) {
            const d = dmap.get(r.ref)

            const el   = create('div')
            el.style.paddingBottom = '0.5em'
            const link = create('a')
            link.href = `https://beepb00p.xyz/exobrain2/${d.file}#${d.id}`
            // todo don't need # in text?
            link.textContent = d.file
            const text = create('span')
            text.style.display = 'block'
            text.textContent = d.text
            el.appendChild(link)
            el.appendChild(text)
            results.appendChild(el)
        }
    }

    search.addEventListener('click', dosearch)

}

document.addEventListener('DOMContentLoaded', ready)
