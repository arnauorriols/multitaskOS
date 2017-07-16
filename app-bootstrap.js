document.addEventListener('DOMContentLoaded', function() {
    // Initialize Firebase
    var config = {
      apiKey: "AIzaSyAotXOS5ekVo-NEfAJ-qBEc0Q1Jv7uxqNo",
      authDomain: "multitaskos-bdde5.firebaseapp.com",
      databaseURL: "https://multitaskos-bdde5.firebaseio.com",
      projectId: "multitaskos-bdde5",
      storageBucket: "multitaskos-bdde5.appspot.com",
      messagingSenderId: "690617050772"
    };
    firebase.initializeApp(config);

		firebase.auth().onAuthStateChanged(function(user) {
			if (user) {
					document.getElementById('logout').classList.remove('hide');
					document.getElementById('login').classList.add('hide');
					database.ref('users-data/' + user.uid).once('value').then(function(data) {
						var model = data.val();
						if (model) {
							localModel = localStorage.getItem(STORAGE_KEY);
							if (!localModel || model.timestamp > localModel.timestamp) {
								model.unsavedJob.worklog = [];  // Firabase does not store emtpy arrays
								if (!model.hasOwnProperty('jobQueue')) {
									model.jobQueue = [];
								}
								model.jobQueue.forEach(function (jobTuple) {
									if (!jobTuple[1].hasOwnProperty('worklog')) {
										jobTuple[1].worklog = [];
									}
								});
								multitaskos.ports.syncModelFromDatabase.send(model);
							}
						}
					});
			} else {
					document.getElementById('logout').classList.add('hide');
					document.getElementById('login').classList.remove('hide');
			}
		});

		document.getElementById('login-google').onclick = function () {
				var provider = new firebase.auth.GoogleAuthProvider();
				firebase.auth().signInWithPopup(provider);
		}

		document.getElementById('logout').onclick = function () {
				firebase.auth().signOut().catch(console.log);
		};
		$('.dropdown-button').dropdown({hover: true});


    function compat(model) {
        for (props of [['thread', 'job'], ['threadQueue', 'jobQueue'], ['newThread', 'newJob']]) {
            var oldProp = props[0];
            var newProp = props[1];
            if (model.hasOwnProperty(oldProp)) {
                model[newProp] = model[oldProp];
                delete model[oldProp];
            }
        }

				// v.1.2.0
			  let migrateJob = (job) => {
						job.title = job.threadName;
						delete job.threadName;
						if (job.worklog) {
								job.journal.unshift(job.worklog);
						}
						job.worklog = job.journal;
						delete job.journal;
						return job;
				}

        for (props of [['newJob', 'unsavedJob']]) {
            var oldProp = props[0];
            var newProp = props[1];
            if (model.hasOwnProperty(oldProp)) {
                model[newProp] = model[oldProp];
                delete model[oldProp];
            }
        }

			  if (model.unsavedJob.hasOwnProperty('threadName')) {
					  model.usavedJob = migrateJob(model.unsavedJob);
				}

				if (model.hasOwnProperty('job')) {
						if (model.job) {
							  model.jobQueue.unshift(model.job);
						}
					  delete model.job;
				}

				if (model.jobQueue.length && model.jobQueue[0].hasOwnProperty('threadName')) {
					  model.jobQueue = model.jobQueue.map(migrateJob);
				}

				if (model.jobQueue.length && !(model.jobQueue[0] instanceof Array)) {
					  model.jobQueue = model.jobQueue.map((job) => ["Queued", job]);
				}

			  if (model.hasOwnProperty('hotkeysPressed')) {
						delete model.hotkeysPressed;
				}
        return model;
    }
		var STORAGE_KEY = 'MultitaskOS-Model';
		storageContent = localStorage.getItem(STORAGE_KEY);
    var savedModel =
        storageContent ?
          compat(JSON.parse(storageContent)):
          null;
    var multitaskos = Elm.Main.embed(document.getElementById('elm-app'), savedModel);
	  var database = firebase.database();
    multitaskos.ports.persistModel.subscribe(function (model) {
				model.timestamp = Date.now();
        localStorage.setItem(STORAGE_KEY, JSON.stringify(model));
				var user = firebase.auth().currentUser;
				if (user) {
					database.ref('users-data/' + user.uid).set(model);
				}
    });
});
