//got info on how to do date from https://stackoverflow.com/questions/10211145/getting-current-date-and-time-in-javascript
Date.prototype.today = function () {
    return ((this.getDate() < 10)?"0":"") + this.getDate() +"/"+(((this.getMonth()+1) < 10)?"0":"") + (this.getMonth()+1) +"/"+ this.getFullYear();
}
Date.prototype.timeNow = function () {
    return ((this.getHours() < 10) ? "0" : "") + this.getHours() + ":" + ((this.getMinutes() < 10) ? "0" : "") + this.getMinutes() + ":" + ((this.getSeconds() < 10) ? "0" : "") + this.getSeconds();
}
const date = new Date().today() +" "+ new Date().timeNow();

//got info on how to format decimals from https://stackoverflow.com/questions/30108219/formatting-numbers-for-commas-and-decimal-places-with-javascript
function formatNumber(num) {
    return parseFloat(num).toFixed(2).toLocaleString();
}

//got info on how to format commas from https://stackoverflow.com/questions/2901102/how-to-format-a-number-with-commas-as-thousands-separators
function numberWithCommas(x) {
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

let endDate = new Date().toISOString().slice(0, 10);
let endDateParts = endDate.split("-");
endDateParts[2] = "01";
let startDate = "";
for (let i = 0; i < endDateParts.length; i++){
    startDate += endDateParts[i];
    if (i < endDateParts.length-1) {
        startDate += "-";
    }
}

const price_message = document.getElementById('price');

const goldHeaders = new Headers();
goldHeaders.append('Accept', 'application/json');
goldHeaders.append('Access-Control-Allow-Origin', '*');
goldHeaders.append('Access-Control-Request-Method', 'GET');
let goldPrice;
let url = 'https://data.nasdaq.com/api/v3/datasets/LBMA/GOLD.json?start_date='+startDate+'&end_date='+endDate+'&api_key=ZjvcsdrtyQaMR-Vzszvz';
fetch(url, {headers: goldHeaders})
    .then(response => {
        response.json()
            .then(jsonObj => {
                let dataset = jsonObj.dataset;
                let data = dataset.data;
                let today = data[0];
                goldPrice = today[1];
                const formattedGoldPrice = numberWithCommas(formatNumber(goldPrice));
                price_message.innerText = 'The price of gold as of '+date+' is $'+formattedGoldPrice+' per troy-ounce';
        })
    })
    .catch(error => {
        console.log(error);
        price_message.innerText = 'Error retrieving gold price from API. View console for more info.';
    });

const submit_button = document.getElementById('submit');
submit_button.addEventListener('click', () => {
    const infoDiv = document.createElement('div');
    const container = document.getElementById('container');
    if (price_message.innerText === 'Fetching the latest price of gold from the market...') {
        infoDiv.setAttribute('class', 'bad');
        infoDiv.innerText = "Please wait for gold price to be retrieved";
        container.prepend(infoDiv);
    } else {
        const value = document.getElementById('value').value;
        const unit = document.getElementById('units');
        const unitValue = unit.value;
        const options = unit.innerText.toString().split('\n');
        const selectedIndex = unit.options.selectedIndex;
        const unitName = options[selectedIndex];

        let measure_list = document.getElementById("measure_list").value;
        const unitValueList = measure_list.trim().replaceAll("\'", "").replace('[', '').replace(']', '').split(',');
        for (let i = 0; i < unitValueList.length; i++) {
            unitValueList[i] = unitValueList[i].trim()
        }

        if (value !== "" && typeof value === "string" && typeof parseFloat(value) === "number" && !Number.isNaN(value) &&
            typeof unitValue === "string" && unitValueList.includes(unitValue) && typeof unitName === "string" && options.includes(unitName)) {

            const conversionHeaders = new Headers();
            conversionHeaders.append('Accept', 'application/json');
            conversionHeaders.append('Access-Control-Allow-Origin', '*');
            conversionHeaders.append('Access-Control-Request-Method', 'GET');
            let url = 'http://'+location.host.toString()+'/unitconv/convert?from='+unitValue.toString()+'&value='+value.toString()+'&to=t_oz';
            fetch(url, {headers: conversionHeaders})
                .then(response => {
                    response.json()
                        .then(jsonObj => {
                            if (Object.keys(jsonObj).length === 1) {
                                infoDiv.setAttribute('class', 'bad');
                                infoDiv.innerText = jsonObj.error;
                            } else {
                                infoDiv.setAttribute('class', 'good');
                                let result = jsonObj.value;
                                let dollarAmount = result*goldPrice;
                                let formattedDollarAmount = numberWithCommas(formatNumber(dollarAmount));
                                infoDiv.innerText = date+" "+value+" "+unitName+" of gold is worth $"+formattedDollarAmount;
                            }
                        })
                    .catch(error => {
                        console.log(error);
                        infoDiv.setAttribute('class', 'bad');
                        infoDiv.innerText = 'Error calculating dollar amount.';
                    })
                })
                .catch(error => {
                    console.log(error);
                    infoDiv.setAttribute('class', 'bad');
                    infoDiv.innerText = 'Error retrieving conversion value from API. View console for more info.';
                }).finally(() => {
                    container.prepend(infoDiv);
                });

        } else {
            infoDiv.setAttribute('class', 'bad');
            infoDiv.innerText = "Invalid input";
            container.prepend(infoDiv);
        }
    }
    infoDiv.addEventListener('click', () => infoDiv.remove());
});
