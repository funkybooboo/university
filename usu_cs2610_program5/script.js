const { createApp } = Vue

createApp({
    computed: {

    },
    data() {
        return {
            loadingMessage: 'Getting your location data...',
            locationMessage: '',
            currentWeatherConditions: {
                message: 'Current weather conditions @ ',
                currentTemp: 'Temp: ',
                high: 'High: ',
                low: 'Low: ',
                sky: 'Sky: ',
                humidity: 'Humidity: ',
                pressure: 'Pressure: '
            },
            likely: 0,
            neutral: 40,
            unlikely: 0,
            forecastData: [],
            date: new Date(),
            latAndLogErrorMessage: '',
            latAndLogError: false,
            currentWeatherErrorMessage: '',
            currentWeatherError: false,
            forecastErrorMessage: '',
            forecastError: false,
            gotAllData: 0
        }
    },
    methods: {
        changeColor(index, currentColor) {
            if (currentColor === 'grey') {
                this.forecastData[index].color = 'green'
                this.neutral--
                this.likely++
            } else if (currentColor === 'green') {
                this.forecastData[index].color  = 'red'
                this.likely--
                this.unlikely++
            } else {
                this.forecastData[index].color  = 'grey'
                this.unlikely--
                this.neutral++
            }
        },

    },
    mounted: function () {
        this.$nextTick(() => {
            // https://stackoverflow.com/questions/10211145/getting-current-date-and-time-in-javascript
            Date.prototype.today = function () {
                return ((this.getDate() < 10)?"0":"") + this.getDate() +"/"+(((this.getMonth()+1) < 10)?"0":"") + (this.getMonth()+1) +"/"+ this.getFullYear();
            }
            Date.prototype.timeNow = function () {
                return ((this.getHours() < 10)?"0":"") + this.getHours() +":"+ ((this.getMinutes() < 10)?"0":"") + this.getMinutes() +":"+ ((this.getSeconds() < 10)?"0":"") + this.getSeconds();
            }
            let latitude
            let longitude
            fetch('https://api.ipbase.com/v2/info?apikey=5IYlnBALMrQhyV0yFnoSxMki8Fj7qpa9fgV7ZtPU')
                .then(response => response.json()
                    .then(json => {
                        let city = json.data.location.city.name
                        let state = json.data.location.region.name
                        let country = json.data.location.country.name
                        latitude = json.data.location.latitude
                        longitude = json.data.location.longitude
                        if (!Number.isNaN(latitude) && !Number.isNaN(longitude)) {
                            if ((city === '' || city === undefined) || (state === '' || state !== undefined) || (country === '' || country === undefined)) {
                                this.locationMessage = 'You are located at coordinates ('+latitude+', '+longitude+')'
                            } else {
                                this.locationMessage = 'You are located in '+city+', '+state+', '+country+' at coordinates ('+latitude+', '+longitude+')'
                            }
                            this.gotAllData += 1
                            fetch('https://api.openweathermap.org/data/2.5/weather?lat='+latitude+'&lon='+longitude+'&appid=34e114e5243ea8aba7668c31de1b3a2f&units=imperial')
                                .then(response => response.json()
                                    .then(json => {
                                        this.currentWeatherConditions.message += this.date.today() + " " + this.date.timeNow()
                                        this.currentWeatherConditions.currentTemp += json.main.temp + ' F'
                                        this.currentWeatherConditions.high += json.main.temp_max + ' F'
                                        this.currentWeatherConditions.low += json.main.temp_min + ' F'
                                        this.currentWeatherConditions.sky += json.weather[0].description
                                        this.currentWeatherConditions.humidity += json.main.humidity + '%'
                                        this.currentWeatherConditions.pressure += json.main.pressure + ' hPa'
                                        this.gotAllData += 1

                                        fetch('https://api.openweathermap.org/data/2.5/forecast?lat='+latitude+'&lon='+longitude+'&appid=34e114e5243ea8aba7668c31de1b3a2f&units=imperial')
                                            .then(response => response.json()
                                                .then(json => {
                                                    let list = json.list
                                                    for (let i = 0; i < list.length; i++) {
                                                        let data = list[i]
                                                        this.forecastData[i] = {
                                                            color: 'grey',
                                                            message: 'Conditions for ' + data.dt_txt,
                                                            temp: 'Temp: ' + data.main.temp + ' F',
                                                            sky: 'Sky: ' + data.weather[0].description,
                                                            humidity: 'Humidity: ' + data.main.humidity + '%',
                                                            pressure: 'Pressure: ' + data.main.pressure + ' hPa'
                                                        }
                                                    }
                                                    if (this.forecastData.length === list.length) {
                                                        this.gotAllData += 1
                                                    }
                                                })
                                                .catch(error => {
                                                    console.log(error)
                                                    this.forecastErrorMessage = 'Error getting forecast data. See console for more info.'
                                                    this.forecastError = true
                                                })
                                            )
                                            .catch(error => {
                                                console.log(error)
                                                this.forecastErrorMessage = 'Error getting forecast data. See console for more info.'
                                                this.forecastError = true
                                            })

                                    })
                                    .catch(error => {
                                        console.log(error)
                                        this.currentWeatherErrorMessage = 'Error getting current weather data. See console for more info.'
                                        this.currentWeatherError = true
                                    })
                                )
                                .catch(error => {
                                    console.log(error)
                                    this.currentWeatherErrorMessage = 'Error getting current weather data. See console for more info.'
                                    this.currentWeatherError = true
                                })

                        } else {
                            this.latAndLogErrorMessage = 'Error getting latitude and longitude data. See console for more info.'
                            this.latAndLogError = true
                        }

                    })
                    .catch(error => {
                        console.log(error)
                        this.latAndLogErrorMessage = 'Error getting latitude and longitude data. See console for more info.'
                        this.latAndLogError = true
                    })
                )
                .catch(error => {
                    console.log(error)
                    this.latAndLogErrorMessage = 'Error getting latitude and longitude data. See console for more info.'
                    this.latAndLogError = true
                })
        })
    }
}).mount('#app')
