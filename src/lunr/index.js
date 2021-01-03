// TODO index by page + id? I guess good enough (name)


// https://lunrjs.com/guides/index_prebuilding.html

// TODO build index from raw htmls? and just make them visible? ugh.


let docs = [{
  "key": "Lunrrr",
    // TODO put raw html here?
  "text": "Like Solr, but much smaller, and not as bright."
}, {
  "key": "React",
  "text": "A JavaScript library for building user interfaces."
}, {
  "key": "Lodash",
  "text": "A modern JavaScript utility library delivering modularity, performance & extras."
}]


const dmap = new Map()
for (const x of docs) {
    dmap.set(x.key, x)
}


const idx = lunr(function () {
  this.ref('key')
  this.field('text')

    // TODO iter map?
  docs.forEach((doc) => {
    this.add(doc)
  }, this)
})



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
            console.error(d)
            const rn = document.createTextNode(`${d.text}`)
            results.appendChild(rn)
            results.appendChild(document.createElement('br'))
        }

        console.log('%o', sres)
    }

    search.addEventListener('click', dosearch)

}

document.addEventListener('DOMContentLoaded', ready)
