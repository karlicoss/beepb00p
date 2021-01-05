const TID = 'settings-timestamps'
function timestampsCheckbox() {
    return document.getElementById(TID)
}

const key2cls = {
    'settings-timestamps': '.timestamp, .property.CREATED',
    'settings-priorities': '.priority',
    'settings-todostates': '.todo, .done',
}

function setState(box, state) {
    const key = box.id
    box.checked = state
    Array.from(document.querySelectorAll(key2cls[key])).map(x => {
        if (state) {
            x.classList.remove('exobrain-hide')
        } else {
            x.classList.add   ('exobrain-hide')
        }
    })
    localStorage.setItem(key, JSON.stringify(state))
}

window.onload = () => {
    for (const [key, cls] of Object.entries(key2cls)) {
        const box = document.getElementById(key)
        let state = localStorage.getItem(key)
        state = state == null ? true : JSON.parse(state)
        // todo a bit annoying that it glitches this way.. but whatever
        setState(box, state)
        // user click
        box.addEventListener('change', event => setState(box, box.checked))
    }
}
