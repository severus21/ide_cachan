let file_to_string path=
	let input = open_in path in
	really_input_string input (in_channel_length input)

