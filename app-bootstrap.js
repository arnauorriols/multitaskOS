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
						database.ref('users-data/' + user.uid).on('value', function(data) {
							var model = data.val();
							if (model) {
								localModel = JSON.parse(localStorage.getItem(STORAGE_KEY));
								if (!localModel || !localModel.timestamp || (model.timestamp > localModel.timestamp)) {
									console.log('Model in database is newer than local version. Syncing...');
									// Firebase does not store empty arrays, we need to manually add them
									if (!model.hasOwnProperty('jobQueue')) {
										model.jobQueue = [];
									}
									model.jobQueue.forEach(function (jobQueueEntry) {
										if (!jobQueueEntry.data.hasOwnProperty('worklog')) {
											jobQueueEntry.data.worklog = [];
										}
										if (!jobQueueEntry.hasOwnProperty('history')) {
											jobQueueEntry.history = {events: []};
										}
										if (!jobQueueEntry.history.hasOwnProperty('events')) {
											jobQueueEntry.history.events = [];
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
			// Mutate model to migrate to the newest schema required
			return model
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
	  multitaskos.ports.resizeFocused.subscribe(function () {
        $(document.activeElement).trigger('autoresize');
    });

    multitaskos.ports.initDirtyDropdown.subscribe(function () {
      var dropdowns = $('.dirty-dropdown');
      dropdowns.dropdown({
        alignment: "right",
        constrainWidth: false,
        belowOrigin: true,
        gutter: -3,
        stopPropagation: true
      });
    })

		multitaskos.ports.readFile.subscribe(function (inputId) {
			const file = document.getElementById(inputId).files[0];
			const fileReader = new FileReader();
			fileReader.onload = function (event) {
				const content = JSON.parse(event.target.result);
				multitaskos.ports.fileRead.send(content);
			};
			fileReader.readAsText(file);
		});

		// Register dummy serviceWorker for install to home screen banner in Android to appear
		if (navigator.serviceWorker) {
			navigator.serviceWorker.register('dummy-service-worker.js', {scope: './'});
		}

		$.get('elm-package.json', null, null, 'json')
			.then(function (data) {
				var version = data.version;
				var styleElement = document.head.appendChild(document.createElement('style'));
				styleElement.innerHTML = '.brand-logo:after { content: "v' + version + '";}';
			});
});
