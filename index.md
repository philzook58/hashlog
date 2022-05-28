---
title: "Hashlog"
---

## Try It Out!
<script src="/hashlog/main.bc.js"> </script>

<script>
function run() {
    var query = document.getElementById("query").value;
    const result = Hashlog.run_string(query);
    console.log(result);
    document.getElementById("result").value = result;
}
//window.run = run;
//run();

function pickerbox(select){
    var xhr = new XMLHttpRequest();
    xhr.open('GET', `/hashlog/test/${select.value}`, true);

    // If specified, responseType must be empty string or "text"
    xhr.responseType = 'text';

    xhr.onload = function () {
        if (xhr.readyState === xhr.DONE) {
            if (xhr.status === 200) {
                //console.log(xhr.response);
                //console.log(xhr.responseText);
                document.getElementById("query").value = xhr.responseText;
            }
        }
    };

    xhr.send(null);
}
window.onload = () => {
    urlParams = new URLSearchParams(window.location.search);
    url_eaxmple = urlParams.get('example');

    picker = document.getElementById("examplepicker")
    if(url_eaxmple != null){
        picker.value = url_eaxmple;
    }
    pickerbox(picker)

    
    }
</script>

<textarea id="query" rows="20" style="width:100%">
</textarea>
<button onclick="run()">Run</button>
<select name="example" onchange="pickerbox(this)" id="examplepicker">
  <option value="path.dl">Basics</option>
</select>
<textarea id="result" rows="20" style="width:100%"> </textarea>

# What is this?

A simple datalog using hashconsing

Github repo: <https://github.com/philzook58/hashlog>