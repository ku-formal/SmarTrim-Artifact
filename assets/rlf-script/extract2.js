module.exports = async function(callback) {
    const fs = require('fs');
    const logFile = process.cwd() + `/transactions.json`;

    // remove log file
    try {
        await fs.promises.unlink(logFile);
        console.log(logFile, 'was deleted');
    } catch (err) {
        // ignore
    }

    console.log(`creating file ${logFile}`);
    const block_num = await web3.eth.getBlockNumber();
    console.log("total number of blocks", block_num);

    for (let i = 1; i <= block_num; i++) {
        const block = await web3.eth.getBlock(i);
        for (let idx = 0; idx < block.transactions.length; idx++) {
            const transaction = await web3.eth.getTransaction(block.transactions[idx]);
            console.log("from: " + transaction.from + " to:" + transaction.to + " Nonce:" + transaction.nonce);
            transaction.gas = parseInt(transaction.gas, 16);
            const tx_json = JSON.stringify(transaction) + "\n";
            fs.appendFileSync(logFile, tx_json);
        }
    }
}
