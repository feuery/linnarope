addEventListener("DOMContentLoaded", _ => {
    document.querySelector('#new_color').addEventListener('click', (e) => {
	let amount_of_inputs = document.querySelectorAll('input[type="color"]').length;
	let new_input = document.createElement('input');
	new_input.name = 'color[]';
	new_input.id = 'color' + (amount_of_inputs + 1);
	new_input.type = 'color';
	
	document.querySelector('#colors').appendChild(new_input);
	e.preventDefault();
	return false;
    })
});
