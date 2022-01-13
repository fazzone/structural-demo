const { contextBridge } = require('electron');
const fs = require('fs/promises')
const path = require('path')
const process = require('process')

function check_relative(p) {
    const rp = path.relative(".", p);
    return rp && !rp.startsWith('..') && !path.isAbsolute(rp);
}


contextBridge.exposeInMainWorld('my_electron_bridge', {
    spit: (function (p, c) {
        return fs.writeFile(rp, c);
    }),
    slurp: (function (p) {
        return fs.readFile(p, {encoding: 'utf-8'});
    }),
    list_dir: function(p) {
        return fs.readdir(p);
    }
    // exit: (function (c) {
    //     app.quit();
    // })
});
