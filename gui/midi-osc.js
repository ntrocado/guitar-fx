var routing = {
    // midi cc vs widget id
    16: 'ctrl',
    7: 'vol'
    // etc
}

var onoff = {
    1: '/FEEDBACK-FM',
    2: '/RANDOM-FM',
    3: '/ONSETS',
    4: '/ZIGZAG',
    6: '/RECORD',
    7: '/GRAINS',
    8: '/HIT',
    9: '/LOOP',
    10: '/STOP',
}

var xyValue = [0, 0]

module.exports = {

    oscInFilter:function(data){
        // Filter incoming osc messages

        var {address, args, host, port} = data

        if (host === 'midi') {

            // MIDI routing !
            if (address === '/control') {

                // assign args to variables
                var [channel, ctrl, value] = args.map(arg=>arg.value)

                if (ctrl === 100) {
		    xyValue[0] = value / 127
		    receive('/SET', '/XY', ...xyValue)
		    return
		} else if (ctrl === 101) {
		    xyValue[1] = value / 127
		    receive('/SET', '/XY', ...xyValue)
		    return
		}
		
                // simple routing table (midi cc vs widget id)
                if (routing[ctrl]) receive('/SET', routing[ctrl], value / 127)

                // note: /SET simulates a user interaction and makes the widget send its osc message
                // but it adds some delay (we wait for the UI to respond)
                // AND it may trigger multiple replies if more than one client are connected.
                // Alternatively, we could do this:
                // send('/osc_address', value / 127)
                // receive('/osc_address', value / 127)
                // Or, to send /SET to a single client:
                // receive('/SET', '/osc_address', value / 127, {clientId: ID})


            }

	    if (address === '/note') {

		var [channel, ctrl, value] = args.map(arg=>arg.value)

		console.log('value : ' + value)
		
		if (onoff[ctrl]) receive('/SET', onoff[ctrl], (value > 0) ? 1 : 0)

	    }

            return // bypass original message

        }


        // return data if you want the message to be processed
        return {address, args, host, port}

    }


}
