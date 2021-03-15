const root = "https://andreimikhailov.com/seminars/sp/string/";
function dateFormat(t) {
  var month = '' + (t.getMonth() + 1),
    day = '' + t.getDate(),
    year = t.getFullYear();

  if (month.length < 2) 
    month = '0' + month;
  if (day.length < 2) 
    day = '0' + day;

  return [year, month, day].join('-');
}
document.addEventListener('DOMContentLoaded', function() {
  function mkHide() {
    const hideMarker = document.createElement("span");
    hideMarker.setAttribute("style","color:red;");
    hideMarker.innerHTML = "\u25a0";
    return hideMarker;
  }
  function mkBusy() {
    const busyMarker = document.createElement("span");
    busyMarker.setAttribute("style","color:gold;");
    busyMarker.innerHTML = "\u25a0";
    return busyMarker;
  }
  function mkFull() {
    const fullMarker = document.createElement("span");
    fullMarker.setAttribute("style","color:green;");
    fullMarker.innerHTML = "\u25a0";
    return fullMarker;
  }
  function mkOwner() {
    const ownerMarker = document.createElement("span");
    ownerMarker.innerHTML = "\u25cb";
    return ownerMarker;
  }

  Array.from(document.getElementsByClassName("tag_hide_full")).forEach(function(e) {e.appendChild(mkHide()); e.appendChild(mkHide());});
  Array.from(document.getElementsByClassName("tag_hide_busy")).forEach(function(e) {e.appendChild(mkHide()); e.appendChild(mkBusy());});
  Array.from(document.getElementsByClassName("tag_hide_hide")).forEach(function(e) {e.appendChild(mkHide()); e.appendChild(mkHide());});
  Array.from(document.getElementsByClassName("tag_busy_full")).forEach(function(e) {e.appendChild(mkBusy()); e.appendChild(mkFull());});
  Array.from(document.getElementsByClassName("tag_busy_busy")).forEach(function(e) {e.appendChild(mkBusy()); e.appendChild(mkBusy());});
  Array.from(document.getElementsByClassName("tag_owner")).forEach(function(e) {e.appendChild(mkOwner());});

  var top = document.createElement("p");
  var ttl = document.createElement("h2")
  ttl.innerHTML = "Seminar registry for Sao Paulo string community"
  var msg1 = document.createElement("h4");
  msg1.innerHTML = "All times are Sao Paulo time zone";
  var msg2 = document.createElement("h4");
  msg2.innerHTML = "some links:";

  var links = document.createElement("ul");

  var resemLi = document.createElement("li")
  var resemA = document.createElement("a");
  resemA.setAttribute("href","https://researchseminars.org");
  resemA.innerHTML = "researchseminars.org";

  resemLi.appendChild(resemA);
  var saifrLi = document.createElement("li")
  var saifrA = document.createElement("a");
  saifrA.setAttribute("href","https://www.ictp-saifr.org/upcoming-seminars-and-activities/");
  saifrA.innerHTML = "SAIFR seminars and activities";
  saifrLi.appendChild(saifrA);

  links.appendChild(resemLi);
  links.appendChild(saifrLi);
  top.appendChild(ttl);
  top.appendChild(msg1);
  top.appendChild(msg2);
  top.appendChild(links);
  document.getElementById("p_header").appendChild(top);
  var weekBtn = document.createElement("button");
  weekBtn.innerHTML = "week ahead";
  weekBtn.onclick = function() {
    var t0 = new Date();
    var t1 = new Date();
    t1.setDate(t1.getDate() + 7);

    window.open(`${root}list?_hasdata=&f1=${dateFormat(t0)}&f2=${dateFormat(t1)}`, "_self");

  }
  document.getElementById("tag_timeinterval").appendChild(weekBtn);
  var monthBtn = document.createElement("button");
  monthBtn.innerHTML = "month ahead";
  monthBtn.onclick = function() {
    var t0 = new Date();
    var t1 = new Date();
    t1.setDate(t1.getDate() + 31);

    window.open(`${root}list?_hasdata=&f1=${dateFormat(t0)}&f2=${dateFormat(t1)}`, "_self");

  }
  document.getElementById("tag_timeinterval").appendChild(monthBtn);
  Array.from(document.getElementsByTagName('tr')).forEach(tr => {
    Array.from(tr.getElementsByClassName('td_date')).forEach(d => {
      Array.from(tr.getElementsByClassName('td_time')).forEach(t => {
        const parsedDate = new Date(`${d.innerText.trim()} ${t.innerText.trim()}`)
        var today = new Date();
        var is_today = ( parsedDate.getTime() - today.getTime() ) < 12*60*60*1000 ;
        var is_today_or_tomorrow = ( parsedDate.getTime() - today.getTime() ) < 36*60*60*1000 ;
        d.innerText = `${parsedDate.toLocaleDateString('en-US',{ weekday : 'short' })} ${d.innerText}`
          if (is_today) { 
            d.style.backgroundColor = "red" ; 
            d.style.color = "white" ; 
          } else if (is_today_or_tomorrow) {
            d.style.backgroundColor = "yellow" ;
          }
        })
      })
    })
  })
