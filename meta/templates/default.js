const TID = 'settings-timestamps';
function timestampsCheckbox() {
    return document.getElementById(TID)
}

window.onload = () => {
    const box = timestampsCheckbox()
    function setState(state) {
        box.checked = state
        const dd = state ? 'initial' : 'none'
        Array.from(document.querySelectorAll('.timestamp-wrapper, .property.CREATED')).map(x => {
            x.style.display = dd
        })
        localStorage.setItem(TID, JSON.stringify(state))
    }

    let state = localStorage.getItem(TID)
    if (state != null) {
        state = JSON.parse(state)
    } else {
        state = true
    }
    // todo a bit annoying that it glitches this way.. but whatever
    setState(state)
    // user click
    box.addEventListener('change', (event) => {
        setState(box.checked)
    })
}
