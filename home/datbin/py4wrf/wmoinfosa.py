def wmostations_sa(site = 'all'):

    stationlist = {
        'Abha': {'icao': 'OEAB', 'lat': '18.2333', 'lon': '42.6500', 'wmoid': '41112', 'elev': '2090'},
        'Al-Ahsa': {'icao': 'OEAH', 'lat': '25.3000', 'lon': '49.4833', 'wmoid': '40420', 'elev': '0179'},
        'Al-Baha': {'icao': 'OEBA', 'lat': '20.3000', 'lon': '41.6333', 'wmoid': '41055', 'elev': '1652'},
        'Al-Qaysumah': {'icao': 'OEPA', 'lat': '28.3167', 'lon': '46.1167', 'wmoid': '40373', 'elev': '0358'},
        'Al-Jouf': {'icao': 'OESK', 'lat': '29.7833', 'lon': '40.1000', 'wmoid': '40361', 'elev': '0689'},
        'Al-Kharj': {'icao': 'OEKJ', 'lat': '24.2500', 'lon': '47.5333', 'wmoid': '40445', 'elev': '0439'},
        'Arar': {'icao': 'OERR', 'lat': '30.9000', 'lon': '41.1333', 'wmoid': '40357', 'elev': '0555'},
        'Bisha': {'icao': 'OEBH', 'lat': '19.9833', 'lon': '42.6167', 'wmoid': '41084', 'elev': '1167'},
        'Dammam': {'icao': 'OEDF', 'lat': '26.4667', 'lon': '49.7833', 'wmoid': '40417', 'elev': '0022'},
        'Dawadmi': {'icao': 'OEDW', 'lat': '24.5000', 'lon': '44.3500', 'wmoid': '40435', 'elev': '0990'},
        'Dhahran': {'icao': 'OEDR', 'lat': '26.2667', 'lon': '50.1500', 'wmoid': '40416', 'elev': '0026'},
        'Gassim': {'icao': 'OEGS', 'lat': '26.3000', 'lon': '43.7667', 'wmoid': '40405', 'elev': '0648'},
        'Gizan': {'icao': 'OEGN', 'lat': '16.9000', 'lon': '42.5833', 'wmoid': '41140', 'elev': '0006'},
        'Guriat': {'icao': 'OEGT', 'lat': '31.4000', 'lon': '37.2667', 'wmoid': '40360', 'elev': '0509'},
        'Hafr Al-Batin': {'icao': 'OEKK', 'lat': '27.9000', 'lon': '45.5333', 'wmoid': '40377', 'elev': '0413'},
        'Hail': {'icao': 'OEHL', 'lat': '27.4333', 'lon': '41.6833', 'wmoid': '40394', 'elev': '1015'},
        'Jeddah': {'icao': 'OEJD', 'lat': '21.5000', 'lon': '39.2000', 'wmoid': '41026', 'elev': '0015'},
        'Jeddah-Airport': {'icao': 'OEJN', 'lat': '21.7000', 'lon': '39.1833', 'wmoid': '41024', 'elev': '0015'},
        'Khamis Mushait': {'icao': 'OEKM', 'lat': '18.3000', 'lon': '42.8000', 'wmoid': '41114', 'elev': '2066'},
        'King Khaled': {'icao': 'OERK', 'lat': '24.9333', 'lon': '46.7167', 'wmoid': '40437', 'elev': '0614'},
        'Madinah': {'icao': 'OEMA', 'lat': '24.5500', 'lon': '39.7000', 'wmoid': '40430', 'elev': '0654'},
        'Makkah': {'icao': 'OEMK', 'lat': '21.4333', 'lon': '39.7667', 'wmoid': '41030', 'elev': '0310'},
        'Najran': {'icao': 'OENG', 'lat': '17.6167', 'lon': '44.4167', 'wmoid': '41128', 'elev': '1212'},
        'Rafha': {'icao': 'OERF', 'lat': '29.6167', 'lon': '43.4833', 'wmoid': '40362', 'elev': '0449'},
        'Riyadh': {'icao': 'OERY', 'lat': '24.7167', 'lon': '46.7333', 'wmoid': '40438', 'elev': '0635'},
        'Sharurah': {'icao': 'OESH', 'lat': '17.4667', 'lon': '47.1167', 'wmoid': '41136', 'elev': '0720'},
        'Tabuk': {'icao': 'OETB', 'lat': '28.3667', 'lon': '36.6000', 'wmoid': '40375', 'elev': '0778'},
        'Taif': {'icao': 'OETF', 'lat': '21.4833', 'lon': '40.5500', 'wmoid': '41036', 'elev': '1478'},
        'Turaif': {'icao': 'OETR', 'lat': '31.6833', 'lon': '38.7333', 'wmoid': '40356', 'elev': '0813'},
        'Wadi': {'icao': 'OEWD', 'lat': '20.5000', 'lon': '45.2000', 'wmoid': '41061', 'elev': '0624'},
        'Wejh': {'icao': 'OEWJ', 'lat': '26.2000', 'lon': '36.4667', 'wmoid': '40400', 'elev': '0020'},
        'Yenbo': {'icao': 'OEYN', 'lat': '24.1500', 'lon': '38.0667', 'wmoid': '40439', 'elev': '0008'}}
        
    if site == 'all':
       return stationlist
    else:
       return stationlist[site]
