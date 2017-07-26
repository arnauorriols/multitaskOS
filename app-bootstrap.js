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
		if (window.firebase) {
			firebase.initializeApp(config);

			firebase.auth().onAuthStateChanged(function(user) {
				if (user) {
						document.getElementById('logout').classList.remove('hide');
						document.getElementById('login').classList.add('hide');
						database.ref('users-data/' + user.uid).once('value').then(function(data) {
							var model = data.val();
							if (model) {
								localModel = localStorage.getItem(STORAGE_KEY);
								if (!localModel || !localModel.timestamp || (model.timestamp > localModel.timestamp)) {
									console.log('Model in database is newer than local version. Syncing...');
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
		}

		$('.dropdown-content').click(function() {
			$('.dropdown-button').dropdown('close');
		});

		var isChromeExtension = window.chrome && chrome.runtime && chrome.runtime.id;
		if (isChromeExtension) {
			document.getElementById('login-twitter').parentElement.classList.add('hide');
			document.getElementById('login-github').parentElement.classList.add('hide');
		}
		document.getElementById('login-google').onclick = function () {
				if (isChromeExtension) {
					chrome.identity.getAuthToken({interactive: true}, function(token) {
						if (token) {
							// Authrorize Firebase with the OAuth Access Token.
							var credential = firebase.auth.GoogleAuthProvider.credential(null, token);
							firebase.auth().signInWithCredential(credential).catch(function(error) {
								// The OAuth token might have been invalidated. Lets' remove it from cache.
								if (error.code === 'auth/invalid-credential') {
									chrome.identity.removeCachedAuthToken({token: token}, function() {
									});
								}
							});
						} else {
							console.error('The OAuth Token was null');
						}
					});
				} else {
					var provider = new firebase.auth.GoogleAuthProvider();
					firebase.auth().signInWithPopup(provider);
				}
		}
		document.getElementById('login-twitter').onclick = function () {
				var provider = new firebase.auth.TwitterAuthProvider();
				firebase.auth().signInWithPopup(provider);
		}
		document.getElementById('login-github').onclick = function () {
				var provider = new firebase.auth.GithubAuthProvider();
				firebase.auth().signInWithPopup(provider);
		}

		document.getElementById('logout').onclick = function () {
				firebase.auth().signOut().catch(console.log);
		};

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

			  if (model.hasOwnProperty('unsavedJob')) {
					delete model['usavedJob'];
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
