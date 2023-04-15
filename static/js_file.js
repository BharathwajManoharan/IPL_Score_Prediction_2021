function myfunc() {
    let bat = document.getElementById('batting').value;
    let ball = document.getElementById('bowling').value;
    let venue = document.getElementById('venue').value;
    let run = document.getElementById('run').value;
    let over = document.getElementById('over').value;
    let wicket = document.getElementById('wicket').value;
    let run_last5 = document.getElementById('run_last5').value;
    let wicket_last5 = document.getElementById('wicket_last5').value;

    if ((bat === "") || (ball === "") || (run === "") || (over === "") || (wicket === "") || (run_last5 === "") || (wicket_last5 === "") ||(venue==="")) {
        Swal.fire(
            'All the field are neccessary',
            'Please fill the empty field',
            'warning'
          );
        

    }
    else if (bat === ball) {
        Swal.fire(
            'You select the same team',
            'Team cannot play with itself....select different teams',
            'error'
          );
        
        
    }
    else if (over == 20) {
        Swal.fire(
            'Match is already complete',
            'Try for another match',
            'success'
        );
    }
    else if (wicket_last5 > wicket) {
        Swal.fire(
            'Check again for wicket in last 5 over',
            'As you select the wicket in last 5 over which is greater than total wicket',
            'info'
        );

    }

    else if (run > 250) {
        Swal.fire(
            'This is extreme value ',
            'Please select the value which is really possible',
            'error'
        );
    }
    else if (wicket ==10){
        Swal.fire(
            'Match is already complete',
            'Team Is All Out',
            'success'
        );
    }
    else if (over<5){
        Swal.fire(
            'You Enter less than 5 over',
            'Please enter after 5 over',
            'success'
        );
    }
    else if(over*15<=run){
        Swal.fire(
            'You Enter the Wrong Runs',
            'As this runs is impossible in this'+ over+'overs',
            'error'
        );

    }   

    else {
        $.ajax({
            type: "POST",
            url: '/predict',
            dataType: 'json',
            contentType: 'application/json',
            data: JSON.stringify({
                'wicket': wicket,
                'run': run,
                'over': over,
                'batting': bat,
                'bowling': ball,
                'run_last5': run_last5,
                'venues':venue,
                'wicket_last5': wicket_last5
            }, null, '\t'),
            success: function (response) {
                window.location.href = response.redirect
            },
            headers: { "X-CSRFToken": "{{ csrf_token}}" },
        });

        console.log(ball);
        console.log(bat);
        console.log(venue);
        console.log(run);
        console.log(over);
        console.log(wicket);
        console.log(run_last5);
        console.log(wicket_last5);
        console.log(typeof(venue));
    }
}
