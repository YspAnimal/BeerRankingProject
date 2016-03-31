var page = require('webpage').create();
page.open('http://www.ratebeer.com/beerstyles/abbey-dubbel/71/', function () {
    console.log(page.content); //page source
    phantom.exit();
});
