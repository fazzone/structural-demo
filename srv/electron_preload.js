const { contextBridge } = require('electron');
const fs = require('fs/promises')
const path = require('path')
const process = require('process')


contextBridge.exposeInMainWorld('my_electron_bridge', {
    spit: (function (p, c) {
        const rp = path.relative(".", p);
        const nested = rp && !rp.startsWith('..') && !path.isAbsolute(rp);
        if (!nested)
            return false;
        return fs.writeFile(rp, c);
    }),
    slurp: (function (p) {
        const rp = path.relative(".", p);
        const nested = rp && !rp.startsWith('..') && !path.isAbsolute(rp);
        if (!nested)
            return false;
        return fs.readFile(p, {encoding: 'utf-8'});
    }),
    // exit: (function (c) {
    //     app.quit();
    // })
});
